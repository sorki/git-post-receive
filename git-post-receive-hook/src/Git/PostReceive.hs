{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Git.PostReceive (
    module Git.PostReceive.Types
  , postReceiveHook
  , postReceiveHookEncode
  , postReceiveHookDecode
  , postReceiveHookShow
  , runInRepo
  , withRepoCheckout
  )
  where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T

import Control.Monad (forM)
import qualified Control.Foldl as Fold

import Data.Git hiding (Commit, Tag)
import Data.Git.Ref (HashAlgorithm, fromHex, toHex)
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.Serialize
import Turtle

import Git.PostReceive.Types

instance Serialize Change
instance Serialize (Commit   ByteString)
instance Serialize (Tag      ByteString)
instance Serialize (LightTag ByteString)
instance Serialize (Batch    ByteString)

data PostInfo = PostInfo {
    oldRev :: ByteString
  , newRev :: ByteString
  , ref    :: ByteString
  } deriving (Eq, Show, Ord)

toPostInfo [oldRev, newRev, ref] = PostInfo{..}
toPostInfo x = error $ "Wrong hook input, expecting 'oldrev newrev ref' but got " ++ show x

infoToChange PostInfo{..} | B.all (=='0') oldRev = Create
infoToChange PostInfo{..} | B.all (=='0') newRev = Delete
infoToChange PostInfo{..} | otherwise            = Update

postReceiveHookShow = B.pack . show <$> postReceiveHook
postReceiveHookEncode = encode <$> postReceiveHook

postReceiveHookDecode :: B.ByteString -> Either String (Batch ByteString)
postReceiveHookDecode = decode

postReceiveHook = do
  infos <- fold (stdin) $ Fold.premap (toPostInfo . (map (B.pack . T.unpack)) . T.words . lineToText) Fold.list
  print infos
  out <- forM infos $ \postInfo@PostInfo{..} -> do
    withCurrentRepo $ \repo -> do
      mobj <- getObject repo (fromHex newRev) True
      commits <- case mobj of
          Just (ObjCommit c) -> do
            case (B.stripPrefix "refs/heads/" ref) of
              Just branch -> do
                forcePushed <- fmap not <$> fold (inshell (T.unwords [
                  "git rev-list"
                  , (T.pack $ B.unpack $ oldRev)
                  , (T.cons '^' (T.pack $ B.unpack $ newRev))
                  ]) empty) $ Fold.null

                let mkCommit rev pInfo c forced =
                      Commit {
                        commitRev = rev
                      , commitBranch = branch
                      , commitChangeType = infoToChange pInfo
                      , commitAuthorName = personName  $ commitAuthor c
                      , commitAuthorMail = personEmail $ commitAuthor c
                      , commitMsg = commitMessage c
                      , commitForced = forced
                      }

                case forcePushed of
                  True -> return $ [mkCommit newRev postInfo c True]
                  False -> do
                    -- list commit hashes with git log oldRev..newRev
                    hashes <- fold (
                      inshell (
                           "git log --pretty=format:%H"
                        <> " "
                        <> (T.pack $ B.unpack $ oldRev)
                        <> ".."
                        <> (T.pack $ B.unpack $ newRev)
                      ) empty) $ Fold.premap
                        (textToRef . lineToText) Fold.list

                    commits <- forM hashes $ \h -> do
                      c' <- getCommit repo h
                      return $ mkCommit (toHex h) postInfo c' False

                    -- in chronological order
                    return $ reverse commits

              Nothing -> do
                putStrLn "Wont handle "
                print ref
                return []

          _ -> return []

      lightTags <- case mobj of
          Just (ObjCommit c) -> do
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

  x <- pwd
  return $ Batch {
      batchRepo      = (B.pack $ T.unpack $ format fp x)
    , batchRepoName  = (B.pack $ T.unpack $ format fp (basename x))
    , batchCommits   = concat $ map (\(c, _, _) -> c) out
    , batchLightTags = concat $ map (\(_, l, _) -> l) out
    , batchTags      = concat $ map (\(_, _, t) -> t) out
    }

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

restorePwd act = do
  old <- pwd
  ret <- act
  cd old
  return ret

runInRepo :: Batch ByteString -> Text -> IO (Text)
runInRepo Batch{..} cmd = restorePwd $ do
  cd (fromText $ T.pack $ B.unpack $ batchRepo)
  (_retcode, out, err) <- shellStrictWithErr cmd empty
  return $ T.concat [out, err]

withRepoCheckout :: Batch ByteString -> IO a -> IO a
withRepoCheckout Batch{..} cmd = restorePwd $ do
  with (mktempdir "/tmp" "git") $ \d -> do
    stdout (inshell
      ("git clone "
      <> (T.pack . B.unpack $ batchRepo)
      <> " "
      <> (format fp d)) empty)

    cd d
    cmd
