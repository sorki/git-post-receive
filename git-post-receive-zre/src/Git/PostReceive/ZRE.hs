{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Git.PostReceive.ZRE (
    defaultCommitsGroup
  , publishZre
  , subscribeZre
  , subscribeZreWith
  ) where

import Data.ByteString (ByteString)

import Git.PostReceive
import Git.PostReceive.Serialize
import Options.Applicative
import Network.ZRE

defaultCommitsGroup = mkGroup "commits"

parseGroup = mkGroup
  <$> strArgument (metavar "GROUP" <> (value $ unGroup defaultCommitsGroup))

publishZre :: IO ()
publishZre = do
  out <- postReceiveHookEncode <$> postReceiveHook
  runZreParse parseGroup $ \group -> do
    zjoin group
    zshout group out

subscribeZre :: (Batch ByteString -> ZRE ()) -> IO ()
subscribeZre handler = subscribeZreWith (pure ()) (const handler)

subscribeZreWith :: Parser t -> (t -> Batch ByteString -> ZRE ()) -> IO ()
subscribeZreWith parser handler = runZreParse withGroupParser $ \(group, rest) -> do
  zjoin group
  zrecvShoutsDecode group postReceiveHookDecode $ \case
    Left err -> zfail "Unable to decode"
    Right b -> handler rest b
  where
    withGroupParser = (,) <$> parseGroup <*> parser
