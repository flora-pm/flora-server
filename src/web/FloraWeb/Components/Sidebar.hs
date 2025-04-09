module FloraWeb.Components.Sidebar where
import FloraWeb.Pages.Templates
import Lucid
import FloraWeb.Components.Utils (text)

sidebar :: FloraHTML
sidebar =
  nav_ [id_ "sidebar"] $ do
    section_ [] $
      h3_ $ text "Admin Panel"
    section_ [] $ do
      h3_ $ text "Packages"
      ul_ [] $ do
        li_ $ a_ [class_ "sidebar-item", href_ "/admin/packages"] "Overview"
        li_ $ a_ [class_ "sidebar-item", href_ "/admin/packages/groups"] "Package Groups"

