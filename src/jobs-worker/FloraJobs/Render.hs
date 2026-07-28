module FloraJobs.Render where

import Commonmark qualified
import Commonmark.Extensions qualified as Commonmark
import Commonmark.Pandoc
import Control.Exception
import Data.Default
import Data.Function
import Data.Text (Text)
import Data.Typeable
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Pandoc.Builder
import Text.Pandoc.Builder qualified as Builder
import Text.Pandoc.Class (PandocPure, runPure)
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Walk
import Text.Pandoc.Writers.HTML qualified as HTML

import Flora.Domain.Import.Types

data ImportedDocument
  = MarkdownSource Text
  | RenderedHtml Text
  deriving stock (Eq, Show)

renderImportedDocument
  :: (Error ImportError :> es, Typeable es)
  => String
  -> ImportedDocument
  -> Eff es Text
renderImportedDocument name = \case
  MarkdownSource bodyText -> renderMarkdown name bodyText
  RenderedHtml bodyText -> renderHtml bodyText

renderMarkdown :: (Error ImportError :> es, Typeable es) => String -> Text -> Eff es Text
renderMarkdown name bodyText = do
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
      Left exception -> throw exception
      Right (y :: Cm () Blocks) ->
        y
          & unCm
          & Builder.toList
          & Pandoc nullMeta
          & writeSanitised

renderHtml :: Error ImportError :> es => Text -> Eff es Text
renderHtml bodyText = liftPandoc (readHtml def bodyText) >>= writeSanitised

writeSanitised :: Error ImportError :> es => Pandoc -> Eff es Text
writeSanitised document =
  sanitizeBalance <$> liftPandoc (HTML.writeHtml5String def (walk shiftHeadingLevel document))

liftPandoc :: Error ImportError :> es => PandocPure a -> Eff es a
liftPandoc = either (Error.throwError . MarkdownRenderingError) pure . runPure

shiftHeadingLevel :: Block -> Block
shiftHeadingLevel (Header n attrs content) = Header (n + 2) attrs content
shiftHeadingLevel x = x
