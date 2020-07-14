{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.PostReceive.Aeson where

import Data.Aeson
import Data.Text (Text)
import Git.PostReceive.Types

instance ToJSON   Change
instance FromJSON Change
instance ToJSON   (Commit Text)
instance FromJSON (Commit Text)
instance ToJSON   (LightTag Text)
instance FromJSON (LightTag Text)
instance ToJSON   (Tag Text)
instance FromJSON (Tag Text)
instance ToJSON   (Batch Text)
instance FromJSON (Batch Text)
