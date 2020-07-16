{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Git.PostReceive.Pretty (
    prettyBatch
  , prettyCommit
  , prettyLightTag
  , prettyTag
  , renderBatch
  , renderBatchDull
  , renderCommit
  , renderCommitDull
  ) where

import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format.ISO8601

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Git.PostReceive.Types

renderBatch :: Pretty a => Batch a -> Text
renderBatch  = renderStrict . layoutPretty defaultLayoutOptions . prettyBatch

renderBatchDull :: Pretty a => Batch a -> Text
renderBatchDull  = renderStrict . unAnnotateS . layoutPretty defaultLayoutOptions . prettyBatch

renderCommit :: Pretty a => Commit a -> Text
renderCommit = renderStrict . layoutPretty defaultLayoutOptions . prettyCommit

renderCommitDull :: Pretty a => Commit a -> Text
renderCommitDull = renderStrict . unAnnotateS . layoutPretty defaultLayoutOptions . prettyCommit

prettyTime :: UTCTime -> Doc ann
prettyTime = brackets . pretty . iso8601Show

prettyBatch Batch{..} = vcat $
  [ annotate (color Magenta) ("batch" <+> pretty batchRepoName <+> angles (pretty batchRepo)) ]
  ++ map prettyCommit batchCommits
  ++ map prettyTag batchTags
  ++ map prettyLightTag batchLightTags

prettyCommit Commit{..} = vcat
  [ annotate (color Yellow) ("commit" <+> pretty commitRev) <+> pretty commitBranch
  , "Author:    " <+> pretty commitAuthorName <+> angles (pretty commitAuthorMail)
  , "Authored:  " <+> prettyTime commitAuthored
  , "Committed: " <+> prettyTime commitCommitted <+> "by" <+> pretty commitCommitterName <+> angles (pretty commitCommitterMail)
  , mempty
  , indent 4 (pretty commitMsg)
  ]

prettyLightTag LightTag{..} =
  "light-tag" <+> pretty lightTagCommit <+> pretty lightTagName

prettyTag Tag{..} =
  "tag" <+> pretty tagRev <+> pretty tagAuthorName <+> angles (pretty tagAuthorMail)
