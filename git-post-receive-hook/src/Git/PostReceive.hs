{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.PostReceive (
    module Git.PostReceive.Types
  , postReceiveHook
  , postReceiveHookShow
  , runInRepo
  , withRepoCheckout
  )
  where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T

import Control.Applicative
import qualified Control.Foldl
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)

import Data.Git hiding (Commit, Tag)
import Data.Git.Ref (HashAlgorithm, fromHex, toHex)
import Data.Git.Repository (getCommitMaybe)
import Data.Git.Storage.Object

import qualified Data.Git

import Data.Hourglass (Elapsed(..), Seconds(..), timeGetElapsed)
import Data.Serialize
import Data.Typeable (Typeable)
import qualified Turtle

import Git.PostReceive.Types
import Git.PostReceive.Serialize
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data PostInfo a = PostInfo {
    oldRev :: a
  , newRev :: a
  , ref    :: a
  } deriving (Eq, Show, Ord, Functor)

postReceiveHookShow :: IO ByteString
postReceiveHookShow = B.pack . show <$> postReceiveHook

cvtTime :: Elapsed -> UTCTime
cvtTime (Elapsed (Seconds seconds)) = posixSecondsToUTCTime . fromIntegral $ seconds

postReceiveHook :: IO (Batch ByteString)
postReceiveHook = do
  infos <- Turtle.fold Turtle.stdin
    $ Control.Foldl.premap
        ( toPostInfo
        . (map (B.pack . T.unpack))
        . T.words
        . Turtle.lineToText
        )
        Control.Foldl.list

  out <- forM infos $ \postInfo@PostInfo{..} -> do
    withCurrentRepo $ \repo -> do
      mobj <- getObject repo (fromHex newRev) True
      commits <- case mobj of
          Just (ObjCommit c) -> do
            case (B.stripPrefix "refs/" ref) of
              Just branch -> do
                let mkCommit rev pInfo cm forced =
                      Commit {
                        commitRev           = rev
                      , commitBranch        = branch
                      , commitChangeType    = infoToChange pInfo
                      , commitAuthorName    = personName  $ commitAuthor cm
                      , commitAuthorMail    = personEmail $ commitAuthor cm
                      , commitAuthored      = cvtTime $ timeGetElapsed $ personTime $ commitAuthor cm
                      , commitCommitterName = personName $ commitCommitter cm
                      , commitCommitterMail = personEmail $ commitCommitter cm
                      , commitCommitted     = cvtTime $ timeGetElapsed $ personTime $ commitCommitter cm
                      , commitMsg           = commitMessage cm
                      , commitForced        = forced
                      }

                case infoToChange postInfo of
                  Create -> return [mkCommit newRev postInfo c False]
                  Delete -> return [mkCommit oldRev postInfo c False]
                  Update -> do
                    forcePushed <- Turtle.fold (Turtle.inshell (T.unwords [
                      "git rev-list"
                      , (T.pack $ B.unpack $ oldRev)
                      , (T.cons '^' (T.pack $ B.unpack $ newRev))
                      ]) empty) $ fmap not Control.Foldl.null

                    case forcePushed of
                      True -> return [mkCommit newRev postInfo c True]
                      False -> do

                        -- list commit hashes with git log oldRev..newRev
                        hashes <- Turtle.fold (
                          Turtle.inshell
                            (  "git log --pretty=format:%H"
                            <> " "
                            <> (T.pack $ B.unpack $ oldRev)
                            <> ".."
                            <> (T.pack $ B.unpack $ newRev)
                            )
                            empty)
                          $ Control.Foldl.premap
                              (textToRef . Turtle.lineToText)
                              Control.Foldl.list

                        commits <- forM hashes $ \h -> do
                          mc' <- getCommitMaybe repo h
                          case mc' of
                            Nothing -> do
                              -- XXX: is this a race condition?
                              -- XXX: how to handle this better?
                              error $ show ("getCommit 404", h)
                              --return []
                            Just c' -> return $ mkCommit (toHex h) postInfo c' False

                        -- in chronological order
                        return $ reverse commits

              Nothing -> do
                putStrLn "Wont handle "
                print ref
                return []

          _ -> return []

      lightTags <- case mobj of
          Just (ObjCommit _c) -> do
            case (B.stripPrefix "refs/tags/" ref) of
              Just name -> do
                return [
                  LightTag {
                    lightTagCommit = newRev
                  , lightTagName = name
                  }]
              Nothing -> return []
          _ -> return []

      tags <- case mobj of
          Just (ObjTag t) -> do
            return [
              Tag {
                tagRev = newRev
              , tagAuthorName = personName $ tagName t
              , tagAuthorMail = personEmail $ tagName t
              }]
          _ -> return []

      return (commits, lightTags, tags)

  x <- Turtle.pwd
  return $ Batch {
      batchRepo      = (B.pack $ T.unpack $ Turtle.format Turtle.fp x)
    , batchRepoName  = (B.pack $ T.unpack $ Turtle.format Turtle.fp (Turtle.basename x))
    , batchCommits   = concat $ map (\(c, _, _) -> c) out
    , batchLightTags = concat $ map (\(_, l, _) -> l) out
    , batchTags      = concat $ map (\(_, _, t) -> t) out
    }

toPostInfo :: Show a => [a] -> PostInfo a
toPostInfo [oldRev, newRev, ref] = PostInfo{..}
toPostInfo x = error $ "Wrong hook input, expecting 'oldrev newrev ref' but got " ++ show x

infoToChange :: PostInfo ByteString -> Change
infoToChange PostInfo{..} | B.all (=='0') oldRev = Create
infoToChange PostInfo{..} | B.all (=='0') newRev = Delete
infoToChange PostInfo{}   | otherwise            = Update

walk :: (Typeable hash, HashAlgorithm hash)
     => Git hash
     -> Text
     -> Text
     -> IO [Data.Git.Commit hash]
walk repo new old = go (textToRef new)
  where
  go rev = do
    c <- getCommit repo rev
    case commitParents c of
      [] -> return []
      (x:_) -> do
        case x == textToRef old of
          True -> return [c]
          False -> go x >>= return . (c:)

textToRef :: HashAlgorithm hash => Text -> Ref hash
textToRef = fromHex . B.pack . T.unpack

refToText :: HashAlgorithm hash => Ref hash -> Text
refToText = T.pack . B.unpack . toHex

restorePwd :: MonadIO m => m b -> m b
restorePwd act = do
  old <- Turtle.pwd
  ret <- act
  Turtle.cd old
  return ret

runInRepo :: Batch Text -> Text -> IO (Text)
runInRepo Batch{..} cmd = restorePwd $ do
  Turtle.cd $ Turtle.fromText batchRepo
  (_retcode, out, errout) <- Turtle.shellStrictWithErr cmd empty
  return $ T.concat [out, errout]

withRepoCheckout :: Batch Text -> IO a -> IO a
withRepoCheckout Batch{..} cmd = restorePwd $ do
  Turtle.with (Turtle.mktempdir "/tmp" "git") $ \dir -> do
    Turtle.stdout (Turtle.inshell
      ("git clone "
      <> batchRepo
      <> " "
      <> (Turtle.format Turtle.fp dir)) empty)

    Turtle.cd dir
    cmd
