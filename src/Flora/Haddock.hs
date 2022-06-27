module Flora.Haddock where

import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy as TL
import qualified Documentation.Haddock.Markup as Haddock
import Documentation.Haddock.Parser (parseParas, toRegular)
import Documentation.Haddock.Types (DocMarkupH (..), Header (..), Hyperlink (..), MetaDoc (..))
import Lucid

import Debug.Trace

renderHaddock :: String -> String
renderHaddock = traceId . TL.unpack . renderText . Haddock.markup termsMarkup . toRegular . _doc . parseParas Nothing

termsMarkup :: DocMarkupH () String (Html ())
termsMarkup =
  Markup
    { markupEmpty = pure mempty
    , markupString = const $ pure mempty
    , markupParagraph = p_
    , markupAppend = liftA2 (<>)
    , markupIdentifier = \s -> code_ (toHtml s)
    , markupIdentifierUnchecked = const $ pure mempty -- should never happen
    , markupModule = const $ pure mempty -- i.e. filter these out
    , markupWarning = div_ [class_ "warning"]
    , markupEmphasis = em_
    , markupBold = strong_
    , markupMonospaced = \s -> if TL.length (renderText s) > 1 then pure mempty else s
    , markupUnorderedList = undefined -- \elements -> ul_ (mapM_ id elements) ,
    , markupOrderedList = undefined -- ol_,
    , markupDefList = \items -> dl_ [] $
        forM_ items $ \(dTerm, detail) -> do
          dt_ [] dTerm
          dd_ [] detail
    , markupCodeBlock = const mempty
    , markupTable = mconcat . toList
    , markupHyperlink = \(Hyperlink urlString mLabel) ->
        let url = Text.pack urlString
            label = maybe url (LText.toStrict . renderText) mLabel
         in a_ [href_ url] (toHtml @Text label)
    , -- TODO: extract main part of hostname
      markupAName = pure mempty
    , markupPic = pure mempty
    , markupMathInline = pure mempty
    , markupMathDisplay = pure mempty
    , markupProperty = pure mempty
    , markupExample = pure mempty
    , markupHeader = \(Header _lvl title) -> title
    }
