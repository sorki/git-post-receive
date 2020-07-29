{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding
import qualified Data.Text.IO

import Git.PostReceive.Types
import Git.PostReceive.Filter
import Git.PostReceive.ZRE
import Git.PostReceive.Pretty

import Options.Applicative

data CatOpts = CatOpts {
    commitsOnly :: Bool
  , dull        :: Bool
  , filtering   :: Filter
  } deriving (Show)

parseCatOptions :: Parser CatOpts
parseCatOptions = CatOpts
   <$> switch (long "commits" <> short 'c')
   <*> switch (long "dull")
   <*> parseFilter

main :: IO ()
main = subscribeZreWith parseCatOptions $ \cfg batch' -> do
  case filterBatch (filtering cfg) (fmap (Data.Text.Encoding.decodeUtf8) batch') of
    Nothing -> return ()
    Just batch -> do
      case commitsOnly cfg of
        True ->
          forM_ (batchCommits batch) $ \c -> liftIO
            $ Data.Text.IO.putStrLn
            $ (if dull cfg then renderCommitDull else renderCommit)
            $ c
        False ->
          liftIO
            $ Data.Text.IO.putStrLn
            $ (if dull cfg then renderBatchDull else renderBatch)
            $ batch
