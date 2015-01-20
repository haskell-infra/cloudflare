{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : CloudFlare
-- Copyright   : (c) Copyright 2014 Austin Seipp, Ricky Elrod
-- License     : BSD3
--
-- Maintainer  : admin@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- High-level interface to CloudFlare
module CloudFlare
       ( -- * Types
         Zone, ZoneID
       , SecurityLevel(..)
       , CacheLevel(..)
       , SiteMode(..)
       , Account

         -- * Functions
       , account
       , setSecurityLevel
       , setCacheLevel
       , getZoneIDs
       , getZoneEntries
       , setDevMode
       , clearCache
       , purgeFile

         -- * Low-level API access
       , postCf

         -- * Lenses
       , entryType
       , entryName
       , entryContent
       ) where

import           Data.Map
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Vector as V
import           Network.Wreq

-- | A @'Zone'@ is a domain name registered and managed by CloudFlare
-- nameservers - example zones might be @haskell.org@, @example.com@
-- and @foobar.net@, each with individually managed records.
type Zone = Text

-- | A @'ZoneID'@ uniquely identifies a Zone inside CloudFlare.
type ZoneID = Integer

-- | @'Zone'@ security levels. Using @'setSecurityLevel'@, you can
-- change the overall security settings of any @'Zone'@ quickly based
-- on what you're seeing.
data SecurityLevel
  = Help   -- ^ \"I'm under attack!\"
  | High   -- ^ High security
  | Medium -- ^ Medium security
  | Low    -- ^ Low security
  | Off    -- ^ Essentially off
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | @'Zone'@ caching levels. These levels reflect the overall
-- optimization settings for a given @'Zone'@, which controls how
-- aggressive CloudFlare is about managing content.
data CacheLevel
  = Aggressive
  | Basic
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | @'Zone'@ site mode. When in @'DevMode'@, the CloudFlare cache is
-- completely bypassed for development purposes.
--
-- Development mode is not permanent, and lasts only for three hours,
-- or until it is toggled off.
data SiteMode
  = DevMode
  | ProductionMode
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | An @'Account'@ token is the combination of your API key plus your
-- email address, and is simply a backwards-compatible wrapper around
-- them both.
data Account
  = Account Text Text
  deriving (Eq, Show, Ord)

data DNSRecord = DNSRecord {
    _entryType :: T.Text
  , _entryName :: T.Text
  , _entryContent :: T.Text
  } deriving (Eq, Show)

makeLenses ''DNSRecord

--------------------------------------------------------------------------------
-- Main API

-- | Create an @'Account'@ which encompasses your login information.
account :: Text -- ^ CloudFlare login email.
        -> Text -- ^ CloudFlare API key
        -> Account
account = Account

-- | Set the security level for a domain.
setSecurityLevel :: Account -> Zone -> SecurityLevel -> IO (Either Text ())
setSecurityLevel a z lvl = postCf k a "sec_lvl" [ "z" := z, "v" := lvlStr lvl ]
  where
    k _ = return ()
    lvlStr :: SecurityLevel -> T.Text
    lvlStr Help   = "help"
    lvlStr High   = "high"
    lvlStr Medium = "med"
    lvlStr Low    = "low"
    lvlStr Off    = "eoff"

-- | Set the caching level for a domain.
setCacheLevel :: Account -> Zone -> CacheLevel -> IO (Either Text ())
setCacheLevel a z lvl = postCf k a "cache_lvl" [ "z" := z, "v" := lvlStr lvl ]
  where
    k _ = return ()
    lvlStr :: CacheLevel -> T.Text
    lvlStr Basic      = "basic"
    lvlStr Aggressive = "agg"

-- | Get the IDs for a particular Domain.
getZoneIDs :: Account -> [Zone] -> IO (Either Text (Map Zone ZoneID))
getZoneIDs a z = postCf k a "zone_check" [ "zones" := T.intercalate "," z ]
  where
    k resp = do
      let v = resp ^? responseBody . key "response" . key "zones" . _Value
      return $ case v of
        Nothing -> Data.Map.empty
        Just x -> case (fromJSON x) of
          Success s -> s
          Error _   -> Data.Map.empty

-- | Get all DNS entries for a particular 'Zone'.
getZoneEntries :: Account -> Zone -> IO (Either Text (V.Vector DNSRecord))
getZoneEntries acc zone = do
  postCf k acc "rec_load_all" [ "z" := zone ]
  where
    k resp = do
      let v = resp ^. responseBody . key "response" . key "recs" . key "objs" . _Array
      V.mapM processRec v
    processRec record = do
      let name = record ^?! key "name" . _String
          value = record ^?! key "content" . _String
          type' = record ^?! key "type" . _String
      return $ DNSRecord type' name value

-- | Set the mode for a particular @'Zone'@ entry. When in
-- @'DevMode'@, the CloudFlare cache is completely bypassed for
-- development purposes.
--
-- Development mode is not permanent, and lasts only for three hours,
-- or until it is toggled off.
setDevMode :: Account -> Zone -> SiteMode -> IO (Either Text ())
setDevMode a z m = postCf k a "devmode" [ "z" := z, "v" := modeStr ]
  where
    k _ = return ()
    modeStr | m == DevMode = "1" :: Text
            | otherwise    = "0"

-- | Purge CloudFlare cache servers of all content. It may take up to
-- 48 hours for the cache to rebuild and optimum performance to be
-- achieved so this function should be used sparingly.
clearCache :: Account -> Zone -> IO (Either Text ())
clearCache a z = postCf k a "fpurge_ts" [ "z" := z, "v" := ("1" :: Text) ]
  where k _ = return ()

-- | Clear a single file from CloudFlare's cache servers.
--
-- Keep in mind, that if an HTTP and an HTTPS version of the file
-- exists, then both versions will need to be purged independently
purgeFile :: Account -> Zone -> Text -> IO (Either Text ())
purgeFile a z url = postCf k a "zone_file_purge" [ "z" := z, "url" := url ]
  where k _ = return ()

--------------------------------------------------------------------------------
-- Internals

-- | Utility to post to Cloudflare API endpoints and pass responses to
-- continuations. Returns 'Left' if an error occurs.
postCf :: (Response Value -> IO b)
       -> Account
       -> Text
       -> [FormParam]
       -> IO (Either Text b)
postCf k (Account e t) a vals = do
  resp <- asValue =<< post cf ([ "email" := e, "tkn" := t, "a" := a ] ++ vals)
  case (checkResp resp) of
    Left err -> return (Left err)
    Right v  -> k v >>= return . Right
  where
    cf = "https://www.cloudflare.com/api_json.html"

    checkResp :: AsValue b => Response b -> Either Text (Response b)
    checkResp resp = do
      let r = resp ^. responseBody . key "result" . _String :: Text
          m = resp ^. responseBody . key "msg"    . _String :: Text
      case (r == "success") of
        True  -> Right resp
        False -> Left $ T.concat ["Cloudflare error: '", m, "'"]
