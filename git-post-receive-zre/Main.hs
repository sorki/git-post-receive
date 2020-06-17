{-# LANGUAGE OverloadedStrings #-}
module Main where

import Git.PostReceive
import Network.ZRE

group = mkGroup "commits"

main = do
  out <- postReceiveHookEncode
  runZre $ do
    zjoin group
    zshout group out
