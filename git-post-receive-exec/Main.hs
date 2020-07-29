{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.Encoding

import Git.PostReceive
import Git.PostReceive.Filter
import Git.PostReceive.ZRE
import Network.ZRE
import Options.Applicative

import System.Exit
import System.IO
import System.Process

data ExecOpts = ExecOpts {
    includeForcePushes :: Bool
  , perCommit          :: Bool
  , execCommand        :: String
  , filtering          :: Filter
  } deriving (Show)

parseExecOptions = ExecOpts
   <$> switch (long "include-force-pushes")
   <*> switch (long "per-commit")
   <*> strOption (long "command")
   <*> parseFilter

main :: IO ()
main = subscribeZreWith parseExecOptions $ \cfg batch' -> do
  case filterBatch (filtering cfg) (fmap (Data.Text.Encoding.decodeUtf8) batch') of
    Nothing -> return ()
    Just b@Batch{..} -> do

      when (
        any commitForced batchCommits == False
        || (any commitForced batchCommits && includeForcePushes cfg)
        ) $ do

            let runner commit = liftIO $ do
                  (_, Just hOut, Just hErr, p) <- createProcess $ (shell (execCommand cfg)) {
                          std_out = CreatePipe
                        , std_err = CreatePipe
                        , env = Just [
                              ("GIT_REPO", Data.Text.unpack batchRepo)
                            , ("GIT_BRANCH", Data.Text.unpack (commitBranch commit))
                            , ("GIT_REV", Data.Text.unpack (commitRev commit))
                            ]
                        }
                  ec <- waitForProcess p
                  out <- hGetContents hOut
                  err <- hGetContents hErr

                  case ec of
                    ExitSuccess -> do
                      putStrLn "Command done"
                      putStrLn $ "Output was:\n" ++ out
                    ExitFailure c -> do
                      putStrLn $ "Command failed with exit code " ++ (show c)
                      putStrLn $ "Output was:\n" ++ out
                      putStrLn $ "Error was:\n" ++ err

            case perCommit cfg of
              True -> mapM_ runner batchCommits
              False -> unless (null batchCommits) $ runner $ last batchCommits
