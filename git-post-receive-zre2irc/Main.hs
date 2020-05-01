{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad

import qualified Data.ByteString.Char8 as B
import qualified Data.List             as L
import qualified Data.Text             as T

import Git.PostReceive
import Data.Text.IRC.Color
import Network.ZRE

ircGroup = "irc"
commitsGroup = "commits"

main :: IO ()
main = runZre $ do
  zjoin commitsGroup
  zjoin ircGroup
  zrecvShouts $ whenDecodes postReceiveHookDecode $ \Batch{..} -> do
    unless (L.null batchCommits) $ do
      zshout ircGroup $ B.unwords [
          bfg cyan batchRepoName
        , bfg teal "received"
        , bfg cyan (B.pack $ show $ length batchCommits)
        , bfg teal "commits"
        ]
      forM_ batchCommits $ \Commit{..} -> do
        zshout ircGroup $ B.concat [
            "["
          , bfg brown commitBranch
          , "] "
          , bfg purple commitAuthorName
          , ": "
          , bfg lime $ B.takeWhile (/='\n') commitMsg
          ]

    unless (L.null batchLightTags) $ do
      forM_ batchLightTags $ \LightTag{..} -> do
        zshout ircGroup $ B.unwords [
            bfg cyan batchRepoName
          , bfg teal "tagged"
          , bfg cyan lightTagName
          ]

-- big fore-ground color
bfg c = B.pack . T.unpack . fg c . T.pack . B.unpack
