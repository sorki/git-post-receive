{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as B
import qualified Data.List             as L
import qualified Data.Text             as T
import qualified Data.Text.Encoding
import qualified Data.Time.Clock

import Git.PostReceive.Types
--import Git.PostReceive.Serialize
import Git.PostReceive.ZRE
import Data.Text.IRC.Color
import Options.Applicative
import Network.ZRE

import Network.IRC.Bridge.Types
import Network.IRC.Bridge.Serialize
--import Network.IRC.Bridge.ZRE

-- XXX
g = mkGroup "ircInput"

data IRCOptions = IRCOptions {
    ircTo     :: IRCTarget
  , ircNotice :: Bool
  } deriving (Show)

parseIRCOptions = IRCOptions
  <$> (IRCChannel <$> strOption (long "chan" <> short 'c'))
  <*> switch (long "notice")

main :: IO ()
main = subscribeZreWith parseIRCOptions $ \cfg Batch{..} -> do
  zjoin g
  let msg body = do
        now <- liftIO $ Data.Time.Clock.getCurrentTime
        zshout g $ encodeIRCOutput $ IRCOutput
          { outputTo = ircTo cfg
          , outputBody = body
          , outputTime = now
          , outputIsNotice = ircNotice cfg
          }

  unless (L.null batchCommits) $ do
    msg $ T.unwords [
         bfg cyan batchRepoName
       , bfg teal "received"
       , bfg cyan (B.pack $ show $ length batchCommits)
       , bfg teal "commits"
       ]
    forM_ batchCommits $ \Commit{..} -> do
       msg $ T.concat [
           "["
         , bfg brown commitBranch
         , "] "
         , bfg purple commitAuthorName
         , ": "
         , bfg lime $ B.takeWhile (/='\n') commitMsg
         ]

  unless (L.null batchLightTags) $ do
    forM_ batchLightTags $ \LightTag{..} -> do
      msg $ T.unwords [
          bfg cyan batchRepoName
        , bfg teal "tagged"
        , bfg cyan lightTagName
        ]
{--
runZre $ do
  zjoin commitsGroup
  zjoin ircGroup
  zrecvShoutsDecode commitsGroup postReceiveHookDecode $ \out -> case out of
    Left err -> zfail "Unable to decode"
    Right Batch{..} -> do
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
--}

-- big fore-ground color
bfg c = fg c . Data.Text.Encoding.decodeUtf8 -- T.pack . B.unpack
