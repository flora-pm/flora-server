module FloraWeb.Components.Footer where

import Lucid

import FloraWeb.Pages.Templates.Types (FloraHTML)
import FloraWeb.Components.Utils

footer :: FloraHTML
footer =
  footer_ [class_ "footer"] $ do
    div_ [class_ "footer-main wrapper"] $
      div_ [class_ "grid grid-4 grid--large"] $ do
        p_ [class_ "footer-slogan"] "A modern package index for Haskell"
        span_ ""
        nav_ [class_ "flow", ariaLabelledby_ "nav-secondary"] $ do
          h2_ [class_ "title-section", id_ "nav-secondary"] "More links"
          ul_ [class_ "flow flow--small", role_ "list"] $ do
            li_ $
              a_ [href_ "/documentation/"] "Documentation"
            li_ $
              a_ [href_ "/packages/"] "All Packages"
            li_ $
              a_ [href_ "/feed/"] "Package Feeds"
            li_ $
              a_ [href_ "/sessions/new"] "Login"
        nav_ [class_ "flow", ariaLabelledby_ "nav-contact"] $ do
          h2_ [class_ "title-section", id_ "nav-contact"] "Get In Touch"
          ul_ [class_ "flow flow--small", role_ "list"] $ do
            li_ $
              a_ [href_ "mailto:moderation@flora.pm"] "Email"
            li_ $
              a_ [href_ "https://functional.cafe/@flora_pm"] "Mastodon"
            li_ $
              a_ [href_ "https://bsky.app/profile/flora.pm"] "Bluesky"
    div_ [class_ "footer-by"] $
      p_ [class_ "wapper"] $ do
        "A project "
        a_ [href_ "https://notbyai.fyi/"] "made by humans"
        " "
        span_ [class_ "quaternary", ariaHidden "true"] "::"
        " "
        a_ [href_ "https://github.com/flora-pm/flora-server"] "Sources"
        " distributed under AGPL license "
        span_ [class_ "quaternary", ariaHidden "true"] "::"
        " Design & Frontend by "
        a_ [href_ "https://guerilla.studio"] "Guérilla.Studio"









