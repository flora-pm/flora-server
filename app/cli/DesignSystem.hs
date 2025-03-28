{-# LANGUAGE QuasiQuotes #-}

module DesignSystem where

import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Either.Extra
import Data.Foldable (forM_)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar.OrdinalDate as Time
import Data.Time.Clock (UTCTime (..))
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.SPDX
import Distribution.Version
import Effectful
import Effectful.Fail
import Effectful.FileSystem
import Env
import Lucid
import PyF (fmt)
import Security.Advisories.Core.HsecId qualified as HsecId
import Security.CVSS

import Advisories.Model.Affected.Types
import Flora.Environment.Config
import Flora.Model.Category
import Flora.Model.Category qualified as Category
import Flora.Model.Package
import Flora.Search
import FloraWeb.Components.AdvisoryListItem qualified as Component
import FloraWeb.Components.Alert qualified as Component
import FloraWeb.Components.CategoryCard qualified as Component
import FloraWeb.Components.PackageListItem qualified as Component
import FloraWeb.Components.PaginationNav qualified as Component
import FloraWeb.Pages.Templates.Types

newtype ComponentName = ComponentName Text
  deriving newtype (Eq, Ord, Show)

newtype ComponentTitle = ComponentTitle Text
  deriving newtype (Eq, Ord, Show)

generateComponents :: (Fail :> es, FileSystem :> es, IOE :> es) => Eff es ()
generateComponents = do
  environment <- liftIO $ Env.parse id parseDeploymentEnv
  assets <- getAssets environment
  forM_ components $ \(filename, title, name, template) -> do
    let html = TL.replace "\"" "\\\"" $ renderHtml assets template
    writeComponent filename title name html

renderHtml :: Assets -> FloraHTML -> TL.Text
renderHtml assets template =
  runIdentity $ runReaderT (renderTextT template) templateEnv
  where
    templateEnv = defaultsToEnv assets defaultTemplateEnv

writeComponent :: IOE :> es => FilePath -> ComponentTitle -> ComponentName -> TL.Text -> Eff es ()
writeComponent filename title name html =
  liftIO $
    ByteString.writeFile
      ("./design/stories/" <> filename <> ".stories.js")
      (storyTemplate title name html)

-- | A component is represented by a 4-tuple of
components :: Vector (FilePath, ComponentTitle, ComponentName, FloraHTML)
components =
  Vector.fromList
    [
      ( "package-list--item"
      , ComponentTitle "Package List"
      , ComponentName "PackageListItem"
      , packageListItemExample
      )
    , ("category-card", ComponentTitle "Category", ComponentName "CategoryCard", categoryCardExample)
    , ("pagination-area", ComponentTitle "Pagination Area", ComponentName "Pagination", paginationExample)
    , ("alerts", ComponentTitle "Alerts", ComponentName "Alert", alertsExample)
    , ("advisory-preview", ComponentTitle "Advisories", ComponentName "AdvisoryPreviews", packageAdvisoriesExample)
    ]

-----------------------
-- Storybook Helpers --
-----------------------

storyTemplate :: ComponentTitle -> ComponentName -> TL.Text -> ByteString
storyTemplate (ComponentTitle title) (ComponentName name) unprocessedHtml =
  let html = TL.replace "\n" " " unprocessedHtml
   in [fmt|
export default {{
  title: "Components/{title}"
}};

export const {name} = () => "{html}"
|]

--------------
-- Examples --
--------------

packageListItemExample :: FloraHTML
packageListItemExample =
  Component.packageListItem
    ( namespaceExample
    , packageNameExample
    , "Basic libraries"
    , mkVersion [4, 16, 0, 0]
    , License (simpleLicenseExpression BSD_3_Clause)
    , Just $ UTCTime (Time.fromMondayStartWeek 2024 32 1) 0
    , Just $ UTCTime (Time.fromMondayStartWeek 2024 60 15) 0
    )

categoryCardExample :: FloraHTML
categoryCardExample =
  Component.categoryCard categoryExample

categoryExample :: Category
categoryExample =
  Category.mkCategory (CategoryId UUID.nil) "Prelude" Nothing "Libraries that provide default imports"

namespaceExample :: Namespace
namespaceExample = Namespace "haskell"

packageNameExample :: PackageName
packageNameExample = PackageName "base"

paginationExample :: FloraHTML
paginationExample = div_ $ do
  div_ $ do
    h4_ "Two buttons"
    Component.paginationNav 62 2 (SearchPackages "text")
  div_ $ do
    h4_ "Previous button"
    Component.paginationNav 32 3 (SearchPackages "text")
  div_ $ do
    h4_ "Next button"
    Component.paginationNav 32 1 (SearchPackages "text")

alertsExample :: FloraHTML
alertsExample = div_ $ div_ $ do
  h4_ "Info alert"
  Component.info "Info alert"
  h4_ "Error alert"
  Component.exception "Error alert!"

packageAdvisoriesExample :: FloraHTML
packageAdvisoriesExample = do
  let advisoryPreviews =
        Vector.fromList
          [ PackageAdvisoryPreview
              { hsecId = fromJust $ HsecId.parseHsecId "HSEC-2023-0009"
              , namespace = Namespace "hackage"
              , packageName = PackageName "git-annex"
              , summary = "git-annex command injection via malicious SSH hostname"
              , fixed = True
              , published = read "2023-07-25 13:25:42 UTC"
              , cvss = fromRight' $ parseCVSS "CVSS:3.0/AV:N/AC:L/PR:N/UI:R/S:U/C:H/I:H/A:H"
              }
          , PackageAdvisoryPreview
              { hsecId = fromJust $ HsecId.parseHsecId "HSEC-2023-0010"
              , namespace = Namespace "hackage"
              , packageName = PackageName "git-annex"
              , summary = "git-annex private data exfiltration to compromised remote"
              , fixed = True
              , published = read "2023-07-25 13:25:42 UTC"
              , cvss = fromRight' $ parseCVSS "CVSS:3.0/AV:N/AC:H/PR:N/UI:N/S:U/C:H/I:N/A:N"
              }
          , PackageAdvisoryPreview
              { hsecId = fromJust $ HsecId.parseHsecId "HSEC-2023-0012"
              , namespace = Namespace "hackage"
              , packageName = PackageName "git-annex"
              , summary = "git-annex checksum exposure to encrypted special remotes"
              , fixed = True
              , published = read "2023-07-25 13:25:42 UTC"
              , cvss = fromRight' $ parseCVSS "CVSS:3.1/AV:N/AC:H/PR:N/UI:N/S:U/C:L/I:N/A:N"
              }
          , PackageAdvisoryPreview
              { hsecId = fromJust $ HsecId.parseHsecId "HSEC-2023-0013"
              , namespace = Namespace "hackage"
              , packageName = PackageName "git-annex"
              , summary = "git-annex plaintext storage of embedded credentials on encrypted remotes"
              , fixed = True
              , published = read "2023-07-25 13:25:42 UTC"
              , cvss = fromRight' $ parseCVSS "CVSS:3.1/AV:L/AC:L/PR:L/UI:N/S:C/C:H/I:H/A:H"
              }
          , PackageAdvisoryPreview
              { hsecId = fromJust $ HsecId.parseHsecId "HSEC-2023-0011"
              , namespace = Namespace "hackage"
              , packageName = PackageName "git-annex"
              , summary = "git-annex GPG decryption attack via compromised remote"
              , fixed = True
              , published = read "2023-07-25 13:25:42 UTC"
              , cvss = fromRight' $ parseCVSS "CVSS:3.0/AV:N/AC:H/PR:N/UI:N/S:U/C:H/I:N/A:N"
              }
          ]
  ul_ [class_ "advisory-list"] $ Vector.forM_ advisoryPreviews (\preview -> Component.advisoryListRow True preview)
