{-# LANGUAGE OverloadedStrings #-}
module Main where

import CloudFlare
import Control.Applicative ((<$>))
import Control.Lens
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Environment
import System.Environment.XDG.BaseDir

-- TODO: This could include other useful things one day. But I'm lazy.

-- TODO: What the TODO above loadConfig says.
-- TODO: Partial function lolol
main :: IO ()
main = do
  query <- T.pack . head <$> getArgs -- TODO: Partial (Pretty error on invalid usage)
  Right entries <- loadConfig >>= flip getZoneEntries "haskell.org" -- TODO: Partial (handle Left)
  let matches = V.filter (\x -> x ^. entryName == query) entries -- TODO: use lens's (filtered)
  V.mapM_ (\x -> TIO.putStrLn $ x ^. entryContent) matches

-- TODO: Make this work for multiple CF accounts instead of just one.
loadConfig :: IO Account
loadConfig = do
  conf <- getUserConfigFile "" "cloudflare"
  accInfo <- fmap T.words $ TIO.readFile conf
  if length accInfo /= 2
    then error $ "Please ensure " <> conf <> " exists and has format: " <>
               "[email] [api token]"
    else return $ account (head accInfo) (accInfo !! 1)
