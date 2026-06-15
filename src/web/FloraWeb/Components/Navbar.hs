module FloraWeb.Components.Navbar where

import Control.Monad.Extra (whenJust)
import Control.Monad.Reader (ask, asks)
import Data.Text (Text)
import Lucid

import Flora.Model.User (User (..), UserFlags (..))
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates.Types

navbar :: FloraHTML
navbar = do
  -- let xData =
  --       [str|
  --   {
  --     updateTheme() {
  --       const customTheme = document.documentElement.getAttribute('data-theme');
  --       const isSystemDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
  --       const applyTheme = (theme) => {
  --         document.documentElement.setAttribute('data-theme', theme);
  --         (async () => { await cookieStore.set('theme', theme) })();
  --       };
  --       switch (customTheme) {
  --         case 'light':
  --           applyTheme('dark');
  --           break;
  --         case 'dark':
  --           applyTheme('light');
  --           break;
  --         default:
  --           isSystemDark ? applyTheme('light') : applyTheme('dark');
  --           break;
  --       }
  --     }
  --   }
  -- \|]

  ActiveElements{aboutNav, packagesNav} <- asks (.activeElements)
  header_
    [class_ "header"]
    $ do
      div_ [class_ "header-container wrapper"] $ do
        a_ [class_ "header-logo", href_ "/", ariaLabel_ "Homepage"] $ do
          brand
        navbarSearch
        nav_ [class_ "header-nav cluster", ariaLabel_ "Main"] $ do
          navBarLink' "/about" "About" aboutNav
          navBarLink' "/categories" "Browse" packagesNav
          userMenu
brand :: FloraHTML
brand = do
  "Flora"
  toHtmlRaw ("&nbsp;" :: Text)
  span_ [class_ "color-tertiary"] ":: "
  span_ [class_ "color-quaternary"] "["
  "Package"
  span_ [class_ "color-quaternary"] "]"

navBarLink
  :: Text
  -- ^ Additional classes
  -> Text
  -- ^ href attribute
  -> Text
  -- ^ label
  -> Bool
  -- ^ is the element active
  -> FloraHTML
navBarLink additionalClasses href label isActive' =
  a_
    [ href_ href
    , class_ ("btn btn--invisible btn--uppercase" <> additionalClasses <> " " <> isActive isActive')
    , isAriaCurrentPage isActive'
    ]
    (text label)

navBarLink' :: Text -> Text -> Bool -> FloraHTML
navBarLink' = navBarLink ""

userMenu :: FloraHTML
userMenu = do
  ActiveElements{adminDashboard} <- asks (.activeElements)
  TemplateEnv{mUser} <- ask
  adminLink adminDashboard mUser
  settingsLink mUser

navbarSearch :: FloraHTML
navbarSearch = do
  flag <- asks (.displayNavbarSearch)
  mContent <- asks (.navbarSearchContent)
  if flag
    then do
      let contentValue =
            case mContent of
              Nothing -> []
              Just content -> [value_ content]
      form_ [class_ "header-search", action_ "/search", method_ "GET", role_ "search"] $ do
        label_ [for_ "search", class_ "sr-only"] "Search a package"
        div_ [class_ "cluster cluster--nowrap cluster--stretch cluster--tiny flex-grow"] $ do
          input_ $
            [ class_ "flex-grow min-w0"
            , id_ "search"
            , type_ "search"
            , name_ "q"
            , placeholder_ "Search a package"
            , autocomplete_ "off"
            , autocorrect_ "off"
            , autocapitalize_ "off"
            , spellcheck_ "off"
            ]
              ++ contentValue
          button_
            [ class_ "btn"
            , type_ "submit"
            , label_ "Search"
            ]
            Icons.lookingGlass
    else pure mempty

adminLink :: Bool -> Maybe User -> FloraHTML
adminLink active (Just user)
  | user.userFlags.isAdmin = navBarLink' "/admin" "Admin" active
adminLink _ _ = ""

settingsLink :: Maybe User -> FloraHTML
settingsLink Nothing = ""
settingsLink _ =
  a_
    [ href_ "/settings/"
    , class_ "btn btn--invisible btn--uppercase"
    , ariaLabel_ "Settings"
    -- TODO: Add active class and aria-current attribute when it's the current page
    ]
    Icons.slidersHorizontal

isActive :: Bool -> Text
isActive True = " active"
isActive False = ""

isAriaCurrentPage :: Bool -> Attributes
isAriaCurrentPage True = ariaCurrent_ "page"
isAriaCurrentPage False = mempty
