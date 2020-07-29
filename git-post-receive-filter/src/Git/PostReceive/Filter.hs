{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Git.PostReceive.Filter where

import Data.Text (Text)
import qualified Data.Text

import Git.PostReceive.Types
import Options.Applicative

import Text.Regex.Posix

data Filter = Filter {
    excludeRefs  :: Maybe Text
  , includeRefs  :: Maybe Text
  , excludeRepos :: Maybe Text
  , includeRepos :: Maybe Text
  } deriving (Eq, Show)

parseFilter :: Parser Filter
parseFilter =
      (Filter
        <$> optional ( strOption (long "exclude-refs" <> short 'e' <> value ("pull/[0-9]+/merge")) )
        <*> optional ( strOption (long "include-refs" <> short 'i') )
        <*> optional ( strOption (long "exclude-repos") )
        <*> optional ( strOption (long "include-repos") )
      )
  <|> (pure emptyFilter <$> switch (long "all"))
  <|> (branchFilter <$> strOption (long "branch"))

emptyFilter :: Filter
emptyFilter = Filter {
    excludeRefs  = Nothing
  , includeRefs  = Nothing
  , excludeRepos = Nothing
  , includeRepos = Nothing
  }

defaultFilter :: Filter
defaultFilter = emptyFilter {
    excludeRefs  = Just "pull/[0-9]+/merge"
  }

branchFilter :: Text -> Filter
branchFilter branch = emptyFilter {
    includeRefs  = Just $ "heads/" <> branch
  }

filterBatch :: Filter -> Batch Text -> Maybe (Batch Text)
filterBatch Filter{..} batch =
    (\b -> case b of
      Nothing -> Nothing
      Just bb -> if isEmptyBatch bb then Nothing else b)
  . maybe id (\x -> batchFilter (matchesTextRe x . batchRepoName)) includeRepos
  . maybe id (\x -> batchFilter (not . matchesTextRe x . batchRepoName)) excludeRepos
  . maybe id (\x -> commitFilter (matchesTextRe x . commitBranch)) includeRefs
  . maybe id (\x -> commitFilter (not . matchesTextRe x . commitBranch)) excludeRefs
  $ Just batch

isEmptyBatch :: Batch a -> Bool
isEmptyBatch Batch{..} = null batchCommits && null batchTags && null batchLightTags

commitFilter :: (Commit a -> Bool) -> Maybe (Batch a) -> Maybe (Batch a)
commitFilter f (Just batch) = Just $ batch { batchCommits = filter f (batchCommits batch) }
commitFilter _ _ | otherwise = Nothing

batchFilter :: (a -> Bool) -> Maybe a -> Maybe a
batchFilter  f (Just batch) | f batch = Just batch
batchFilter  _ _ | otherwise = Nothing

matchesRe :: String -> String -> Bool
matchesRe x re = x =~ re

matchesTextRe :: Text -> Text -> Bool
matchesTextRe body = flip matchesRe (Data.Text.unpack body) . Data.Text.unpack
