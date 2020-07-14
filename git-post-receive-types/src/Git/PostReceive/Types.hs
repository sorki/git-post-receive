{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Git.PostReceive.Types where

import Data.Time.Clock (UTCTime)
import GHC.Generics

data Change = Update | Create | Delete
  deriving (Eq, Show, Read, Ord, Generic)

data Commit a =
  Commit
    { commitRev           :: a
    , commitBranch        :: a
    , commitChangeType    :: Change
    , commitAuthorName    :: a
    , commitAuthorMail    :: a
    , commitAuthored      :: UTCTime
    , commitCommitterName :: a
    , commitCommitterMail :: a
    , commitCommitted     :: UTCTime
    , commitMsg           :: a
    , commitForced        :: Bool
    }
  deriving (Eq, Show, Read, Generic, Functor, Foldable, Traversable)

data LightTag a =
  LightTag
    { lightTagCommit   :: a
    , lightTagName     :: a
    }
  deriving (Eq, Show, Read, Generic, Functor, Foldable, Traversable)

data Tag a =
  Tag
    { tagRev           :: a
    , tagAuthorName    :: a
    , tagAuthorMail    :: a
    }
  deriving (Eq, Show, Read, Generic, Functor, Foldable, Traversable)

data Batch a =
  Batch
    {
      batchRepo        :: a
    , batchRepoName    :: a
    , batchCommits     :: [Commit a]
    , batchTags        :: [Tag a]
    , batchLightTags   :: [LightTag a]
    }
  deriving (Eq, Show, Read, Generic, Functor, Foldable, Traversable)
