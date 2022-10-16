module FloraWeb.Templates.Haddock where

import Control.Monad (forM_)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Distribution.ModuleName (ModuleName)
import Distribution.Text (simpleParse)
import Documentation.Haddock.Markup qualified as Haddock
import Documentation.Haddock.Parser qualified as Haddock
import Documentation.Haddock.Types (DocMarkupH (..), Example (..), Header (..), Hyperlink (..), MetaDoc (..), ModLink (..), Picture (..), Table (..))
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
    , markupTable = \(Table h r) -> mempty -- makeTable h r
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

-- makeTable :: [TableRow FloraHTML] -> [TableRow FloraHTML] -> FloraHTML
-- makeTable headers body = table_ $ do
--   th_ [] $ do
--       makeHeader headers
--       makeCells body

-- makeHeader :: [TableRow FloraHTML] -> FloraHTML
-- makeHeader headers | null header = mempty
--                    | otherwise =
--                       let header = head headers
--                        in thead_ [] $
-- makeTable hs bs = table_ (concatHtml (hs' ++ bs'))
--   where
--     hs' | null hs   = []
--         | otherwise = [thead (concatHtml (map (makeTableRow th) hs))]

--     bs' = [tbody (concatHtml (map (makeTableRow td) bs))]

-- makeTableRow :: (FloraHTML -> FloraHTML) -> TableRow FloraHTML -> FloraHTML
-- makeTableRow tableRow (TableRow cs) = tr_ (concatHtml (map (makeTableCell thr) cs))

-- makeTableCell :: (FloraHTML -> FloraHTML) -> TableCell FloraHTML -> FloraHTML
-- makeTableCell thr (TableCell i j c) = th_ c (i' <> j')
--   where
--     i' = [colspan_ (display i) | i /= 1]
--     j' = [rowspan_ (display j) | j /= 1]

namedAnchor :: String -> FloraHTML -> FloraHTML
namedAnchor n = a_ [name_ (Text.pack $ escapeStr n)]

escapeStr :: String -> String
escapeStr = escapeURIString isUnreserved
