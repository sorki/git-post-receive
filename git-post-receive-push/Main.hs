{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T

import Git.PostReceive
import Network.ZRE
import Turtle

group = "commits"

main = runZre $ do
  zjoin group
  zrecvShouts $ whenDecodes postReceiveHookDecode $ \b@Batch{..} -> do
    unless (any commitForced batchCommits) $ do
      when (not . null $ batchCommits) $ do
        let branch = commitBranch $ head $ batchCommits
        void $ liftIO $ runInRepo b $ "git push origin " <> (T.pack $ B.unpack branch)
        zshout group "Pushed to origin"
