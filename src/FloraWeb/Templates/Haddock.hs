module FloraWeb.Templates.Haddock where

import Control.Monad (forM_)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Distribution.ModuleName (ModuleName)
import Distribution.Text (simpleParse)
import Documentation.Haddock.Markup qualified as Haddock
import Documentation.Haddock.Parser qualified as Haddock
import Documentation.Haddock.Types (DocMarkupH (..), Example (..), Header (..), Hyperlink (..), MetaDoc (..), ModLink (..), Picture (..), Table (..), TableCell (..), TableRow (..))
import Flora.Model.Package (PackageName (..))
import FloraWeb.Templates (FloraHTML)
import Lucid
import Network.URI

renderHaddock :: PackageName -> Text -> FloraHTML
renderHaddock (PackageName package) input = do
  let metaDoc = Haddock.parseParas (Just (Text.unpack package)) (Text.unpack input)
  let normalisedDoc = Haddock.toRegular metaDoc._doc
  Haddock.markup (htmlMarkup (const Nothing)) normalisedDoc

htmlMarkup :: (ModuleName -> Maybe Text) -> DocMarkupH mod String FloraHTML
htmlMarkup modResolv =
  Markup
    { markupEmpty = mempty
    , markupString = toHtml
    , markupParagraph = p_
    , markupAppend = (<>)
    , markupIdentifier = code_ . toHtml
    , markupIdentifierUnchecked = const $ code_ [] (toHtml @Text "FIXME") -- should never happen
    , markupModule = mkModLink
    , markupWarning = div_ [class_ "warning"]
    , markupEmphasis = em_
    , markupBold = strong_
    , markupMonospaced = code_
    , markupUnorderedList = \listItems -> ul_ [] $ forM_ listItems $ \item -> li_ item
    , markupOrderedList = \listItems -> ol_ [] $ forM_ listItems $ \(_, item) -> li_ [] item
    , markupDefList = \listItems ->
        dl_ [] $
          forM_
            listItems
            ( \(dTerm, definition) -> do
                dt_ [] dTerm
                dd_ [] definition
            )
    , markupCodeBlock = pre_ []
    , markupHyperlink = \(Hyperlink strUrl mLabel) ->
        let url = Text.pack strUrl
         in a_ [href_ url] $ fromMaybe (toHtml url) mLabel
    , markupAName = (`namedAnchor` "")
    , markupPic = \(Picture uri mTitle) -> i_ [src_ (Text.pack uri), title_ (maybe "" Text.pack mTitle)] ""
    , markupMathInline = \mathjax -> toHtml ("\\(" ++ mathjax ++ "\\)")
    , markupMathDisplay = \mathjax -> toHtml ("\\[" ++ mathjax ++ "\\]")
    , markupProperty = pre_ . toHtml
    , markupExample = examplesToHtml
    , markupHeader = \(Header l t) -> makeHeader l t
    , markupTable = \(Table h r) -> makeTable h r
    }
  where
    makeHeader :: Int -> FloraHTML -> FloraHTML
    makeHeader 1 mkup = h2_ mkup
    makeHeader 2 mkup = h3_ mkup
    makeHeader 3 mkup = h4_ mkup
    makeHeader 4 mkup = h5_ mkup
    makeHeader _ mkup = h6_ mkup

    examplesToHtml :: [Example] -> FloraHTML
    examplesToHtml examples = pre_ [class_ "screen"] $ forM_ examples $ \e -> exampleToHtml e

    exampleToHtml :: Example -> FloraHTML
    exampleToHtml (Example expression result) = do
      code_ [class_ "prompt"] ">>>"
      strong_ [class_ "userinput"] $ do
        code_ [] (pure $ Text.pack $ expression <> "\n")
        forM_ result (\resultLine -> pure $ Text.pack resultLine)

    mkModLink :: ModLink FloraHTML -> FloraHTML
    mkModLink (ModLink name _mLabel) =
      case extractModInfo (Text.pack name) of
        Nothing -> code_ (toHtml name)
        Just html -> html

    extractModInfo :: Text -> Maybe FloraHTML
    extractModInfo name = do
      modname <- simpleParse (Text.unpack name)
      modUrl <- modResolv modname
      pure $
        span_ [class_ "module"] $
          a_ [href_ modUrl] (toHtml name)

    makeTable :: [TableRow FloraHTML] -> [TableRow FloraHTML] -> FloraHTML
    makeTable headers cells = table_ $ do
      thead_ $ do
        forM_ headers $ \(TableRow cs) -> tr_ [] $ forM_ cs $ \cell -> makeHeaderCell cell
        forM_ cells $ \(TableRow cs) -> tr_ [] $ forM_ cs $ \cell -> makeDataCell cell

    makeHeaderCell :: TableCell FloraHTML -> FloraHTML
    makeHeaderCell (TableCell colSpan rowSpan content) =
      th_ attrs content
      where
        attrs = i <> j
        i = [colspan_ (display colSpan) | colSpan /= 1]
        j = [rowspan_ (display rowSpan) | rowSpan /= 1]

    makeDataCell :: TableCell FloraHTML -> FloraHTML
    makeDataCell (TableCell colSpan rowSpan content) =
      td_ [colspan_ (display colSpan), rowspan_ (display rowSpan)] content

namedAnchor :: String -> FloraHTML -> FloraHTML
namedAnchor n = a_ [name_ (Text.pack $ escapeStr n)]

escapeStr :: String -> String
escapeStr = escapeURIString isUnreserved
