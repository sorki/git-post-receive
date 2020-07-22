{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified Data.ByteString.Char8

import Git.PostReceive.Types
import Git.PostReceive.ZRE
import Git.PostReceive.Pretty
import Network.ZRE

import Options.Applicative

data CatOpts = CatOpts {
    commitsOnly :: Bool
  , dull        :: Bool
  , refs        :: [String]
  , repos       :: [String]
  } deriving (Show)

parseCatOptions = CatOpts
   <$> switch (long "commits" <> short 'c')
   <*> switch (long "dull")
   <*> many (strOption (long "branch" <> short 'b' <> metavar "BRANCH"))
   <*> many (strArgument (metavar "REPO"))

main :: IO ()
main = subscribeZreWith parseCatOptions $ \cfg batch' -> do
  let batch = case refs cfg of
        [] -> batch'
        _  -> filterBatchBranches batch' (refs cfg)

  when (null (repos cfg) || (Data.ByteString.Char8.unpack $ batchRepo batch) `elem` (repos cfg)) $ do
      liftIO
        $ Data.Text.IO.putStrLn
        $ renderBatch
        $ fmap (Data.Text.Encoding.decodeUtf8)
        $ batch

-- XXX: filtering needs to go elsewhere
-- as it's needed by e.g. zre2irc too
filterBatchBranches batch refs = batch { batchCommits = filter (f refs) (batchCommits batch) }
  where f rs Commit{..} = (Data.ByteString.Char8.unpack commitBranch) `elem` (map ("heads/"++) rs)
