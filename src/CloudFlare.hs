{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : CloudFlare
-- Copyright   : (c) Copyright 2014 Austin Seipp
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : GHC
--
-- High-level interface to CloudFlare
module CloudFlare
       ( -- * Types
         Zone, ZoneID
       , SecurityLevel(..)
       , CacheLevel(..)
       , Account

         -- * Functions
       , account
       , setSecurityLevel
       , setCacheLevel
       , getZoneIDs
       ) where

import           Data.Map
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
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

-- | An @'Account'@ token is the combination of your API key plus your
-- email address, and is simply a backwards-compatible wrapper around
-- them both.
data Account
  = Account Text Text
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Main API

-- | Create an @'Account'@ which encompasses your login information.
account :: Text -- ^ CloudFlare login email.
        -> Text -- ^ CloudFlare API key
        -> Account
account = Account

-- | Set the security level for a domain.
setSecurityLevel :: Account -> Zone -> SecurityLevel -> IO (Either Text ())
setSecurityLevel a z lvl = postCf k a "sec_lvl" [ "z" := z, "v" := lvlStr ]
  where
    lvlStr | lvl == Help   = "help" :: Text
           | lvl == High   = "high"
           | lvl == Medium = "med"
           | lvl == Low    = "low"
           | lvl == Off    = "eoff"
           | otherwise     = "IMPOSSIBLE" -- warning suppression

    k _ = return ()

-- | Set the caching level for a domain.
setCacheLevel :: Account -> Zone -> CacheLevel -> IO (Either Text ())
setCacheLevel a z lvl = postCf k a "cache_lvl" [ "z" := z, "v" := lvlStr ]
  where
    lvlStr | lvl == Basic      = "basic" :: Text
           | lvl == Aggressive = "agg"
           | otherwise         = "IMPOSSIBLE" -- warning suppression

    k _ = return ()


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
