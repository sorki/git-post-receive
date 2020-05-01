{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B

import Git.PostReceive
import Network.ZRE

group = "commits"

main = runZre $ do
  zjoin group
  zrecvShouts $ whenDecodes postReceiveHookDecode $ \Batch{..} -> do
    forM_ batchCommits $ \Commit{..} -> do
      liftIO $ B.putStrLn $ B.unwords
        [batchRepoName, commitRev]
