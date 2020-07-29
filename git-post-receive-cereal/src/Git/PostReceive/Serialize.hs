{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.PostReceive.Serialize
  ( postReceiveHookEncode
  , postReceiveHookDecode
  ) where

import Data.ByteString (ByteString)
import Data.Serialize
import Data.Time.Clock.Serialize ()

import Git.PostReceive.Types

instance Serialize Change
instance Serialize (Commit   ByteString)
instance Serialize (Tag      ByteString)
instance Serialize (LightTag ByteString)
instance Serialize (Batch    ByteString)

postReceiveHookEncode :: Batch ByteString -> ByteString
postReceiveHookEncode = encode

postReceiveHookDecode :: ByteString -> Either String (Batch ByteString)
postReceiveHookDecode = decode
