{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.List             as L
import qualified Data.Text             as T
import qualified Data.Text.Encoding
import qualified Data.Time.Clock

import Git.PostReceive.Types
import Git.PostReceive.Filter
import Git.PostReceive.ZRE

import Data.Text.IRC.Color
import Options.Applicative

import Network.IRC.Bridge.Types
import Network.IRC.Bridge.Serialize
--import Network.IRC.Bridge.ZRE

import Network.ZRE

-- XXX
g :: Group
g = mkGroup "ircInput"

data IRCOptions = IRCOptions {
    ircTo     :: IRCTarget
  , ircNotice :: Bool
  , filtering :: Filter
  } deriving (Show)

parseIRCOptions :: Parser IRCOptions
parseIRCOptions = IRCOptions
  <$> (IRCChannel <$> strOption (long "chan" <> short 'c'))
  <*> switch (long "notice")
  <*> parseFilter

main :: IO ()
main = subscribeZreWith parseIRCOptions $ \cfg batch' -> do
  case filterBatch (filtering cfg) (fmap (Data.Text.Encoding.decodeUtf8) batch') of
    Nothing -> return ()
    Just Batch{..} -> do
      -- bad as we join on every msg, shouldn't matter..
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
             fg cyan batchRepoName
           , fg teal "received"
           , fg cyan (T.pack $ show $ length batchCommits)
           , fg teal "commits"
           ]

        forM_ batchCommits $ \Commit{..} -> do
           msg $ T.concat [
               "["
             , fg brown commitBranch
             , "] "
             , if commitForced then "(force-pushed) " else ""
             , fg cyan (T.take 11 commitRev)
             , " "
             , fg purple commitAuthorName
             , ": "
             , fg lime $ T.takeWhile (/='\n') commitMsg
             ]

      unless (L.null batchLightTags) $ do
        forM_ batchLightTags $ \LightTag{..} -> do
          msg $ T.unwords [
              fg cyan batchRepoName
            , fg teal "tagged"
            , fg cyan lightTagName
            ]

      unless (L.null batchTags) $ do
        forM_ batchTags $ \Tag{..} -> do
          msg $ T.unwords [
              fg cyan batchRepoName
            , fg teal "tagged"
            , fg cyan tagRev
            , " by "
            , fg purple tagAuthorName
            ]
