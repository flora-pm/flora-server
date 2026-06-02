module FloraWeb.Components.PackageCard where

import Control.Monad
import Control.Monad.Extra (whenJust)
import Data.Fixed (Pico, div')
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time qualified as Time
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version (Version)
import Lucid

import Flora.Model.Package.Types (Namespace, PackageName)
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Pages.Templates (FloraHTML)
import Lucid.Orphans ()

data PackageCardProps = PackageCardProps
  { link :: Text
  , namespace :: Namespace
  , name :: PackageName
  , synopsis :: Text
  , mVersion :: Maybe Version
  , mLastUploadedAt :: Maybe UTCTime
  , mLicense :: Maybe SPDX.License
  , exactMatch :: Bool
  }

packageCard :: UTCTime -> PackageCardProps -> FloraHTML
packageCard now PackageCardProps{link, namespace, name, synopsis, mVersion, mLastUploadedAt, mLicense, exactMatch} =
  a_
    [ class_ ("entityCard" <> (if exactMatch then " entityCard--highlighted" else ""))
    , href_ link
    ]
    $ do
      div_ [] $ do
        span_ [] $ do
          span_ [class_ "entityCard-prefix"] ("@" <> toHtml namespace <> toHtmlRaw ("&ThinSpace;" :: Text) <> "/" <> toHtmlRaw ("&ThinSpace;" :: Text))
          span_ [class_ "entityCard-title"] (toHtml name)
          " "
        unless (Text.null synopsis) $
          span_ [class_ "entityCard-synopsis"] (toHtml synopsis)
        when exactMatch $ do
          " "
          span_ [class_ "badge badge--brand"] "Exact match"
      ul_ [class_ "cluster color-secondary text-small", role_ "list"] $ do
        whenJust mVersion $ \version ->
          li_ $ do
            span_ [class_ "color-tertiary"] Icons.tag
            span_ [class_ "sr-only"] "Version: "
            toHtml version
        whenJust mLastUploadedAt $ \timestamp ->
          li_ $ do
            span_ [class_ "color-tertiary"] Icons.cloudUpload
            span_ [class_ "sr-only"] "Last uploaded: "
            time_ [datetime_ (display (Time.formatTime Time.defaultTimeLocale "%a, %_d %b %Y, %R %EZ" timestamp))] (toHtml $ formatUploadTime timestamp now)
        whenJust mLicense $ \license ->
          li_ $ do
            span_ [class_ "color-tertiary"] Icons.scale
            span_ [class_ "sr-only"] "License: "
            toHtml license

formatUploadTime
  :: UTCTime
  -> UTCTime
  -> Text
formatUploadTime timestamp now =
  let diff = now `Time.diffUTCTime` timestamp
   in Text.pack (toRelativeHumanTime diff)

toRelativeHumanTime :: NominalDiffTime -> String
toRelativeHumanTime diff
  | diff < seconds 30 = "just now"
  | diff < minutes 2 = "1 minute ago"
  | diff < hours 1 = Time.formatTime Time.defaultTimeLocale "%M minutes ago" diff
  | diff < hours 24 = Time.formatTime Time.defaultTimeLocale "%H hours ago" diff
  | diff < days 7 = Time.formatTime Time.defaultTimeLocale "%D days ago" diff
  | diff < days 14 = Time.formatTime Time.defaultTimeLocale "1 week ago" diff
  | diff < months 1 = Time.formatTime Time.defaultTimeLocale "%w weeks ago" diff
  | diff < months 2 = Time.formatTime Time.defaultTimeLocale "1 month ago" diff
  | diff < months 12 = show @Int (diff `div'` months 1) <> " months ago"
  | diff < years 2 = "about 1 year ago"
  | otherwise = show @Int (diff `div'` years 1) <> " years ago"

seconds :: Pico -> NominalDiffTime
seconds = Time.secondsToNominalDiffTime

minutes :: Pico -> NominalDiffTime
minutes n = 60 * seconds n

hours :: Pico -> NominalDiffTime
hours n = 60 * minutes n

days :: Pico -> NominalDiffTime
days n = 24 * hours n

months :: Pico -> NominalDiffTime
months n = 30 * days n

years :: Pico -> NominalDiffTime
years n = 12 * months n
