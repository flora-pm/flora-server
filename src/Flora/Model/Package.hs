module Flora.Model.Package
  ( module Flora.Model.Package.Types,
    formatPackage,
  )
where

import Data.Text (Text)
import Data.Text.Display (display)
import Flora.Model.Package.Types

formatPackage :: Namespace -> PackageName -> Text
formatPackage namespace packageName = "@" <> display namespace <> "/" <> display packageName
