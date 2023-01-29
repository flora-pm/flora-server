module Flora.OddJobs.Render where

import Commonmark qualified
import Commonmark.Extensions qualified as Commonmark
import Control.Exception
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Flora.OddJobs.Types (OddJobException (..))
import Lucid (Html, toHtmlRaw)

renderMarkdown :: String -> Text -> Html ()
renderMarkdown name bodyText = do
  htmlTxt <- do
    let extensions =
          mconcat
            [ Commonmark.mathSpec
            , -- all gfm extensions apart from pipeTable
              Commonmark.emojiSpec
            , Commonmark.strikethroughSpec
            , Commonmark.autolinkSpec
            , Commonmark.autoIdentifiersSpec
            , Commonmark.taskListSpec
            , Commonmark.footnoteSpec
            , -- default syntax
              Commonmark.defaultSyntaxSpec
            , Commonmark.autoIdentifiersSpec
            , Commonmark.implicitHeadingReferencesSpec
            , -- pipe table spec. This has to be after default syntax due to
              -- https://github.com/jgm/commonmark-hs/issues/95
              Commonmark.pipeTableSpec
            ]
    Commonmark.commonmarkWith extensions name bodyText
      >>= \case
        Left exception -> throw (MarkdownFailed exception)
        Right (y :: Commonmark.Html ()) -> pure $! Commonmark.renderHtml y

  toHtmlRaw @Text $! TL.toStrict htmlTxt
