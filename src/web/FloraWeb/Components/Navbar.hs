module FloraWeb.Components.Navbar where

import Control.Monad.Reader (ask, asks)
import Data.Text (Text)
import Data.Text.Display (display)
import Lucid
import Lucid.Alpine

import Flora.Model.PersistentSession (PersistentSessionId (..))
import Flora.Model.User (User (..), UserFlags (..))
import FloraWeb.Components.Utils
import FloraWeb.Templates.Types

navbar :: FloraHTML
navbar = do
  ActiveElements{aboutNav, packagesNav} <- asks activeElements
  let menuClasses =
        "md:flex flex md:items-center "
          <> "flex flex-col md:flex-row absolute md:relative top-[100%] left-0 w-full md:w-auto md:top-0"

  nav_ [class_ "top-navbar", xData_ "{menuOpen: false}"] $! do
    div_ [class_ "navbar-content"] $! do
      div_ [class_ "navbar-left"] $! do
        brand
        navbarSearch

      div_ [class_ (menuClasses <> " navbar-right"), xBind_ "class" "!menuOpen ? 'hidden' : ''"] $! do
        navBarLink " main-page-button" "/" "Search on Flora" False
        navBarLink' "/about" "About" aboutNav
        navBarLink' "/categories" "Categories" packagesNav
        navBarLink' "/packages" "Packages" packagesNav
        -- userMenu
        themeToggle

brand :: FloraHTML
brand = do
  -- Don't touch the .hidden
  div_ [class_ "hidden brand"] $
    link defaultLinkOptions{href = "/", classes = "", childNode = text "Flora :: [Package]"}
  div_ [class_ "brand-menu", xOn_ "click.prevent" "menuOpen = !menuOpen"] $
    link defaultLinkOptions{href = "/", classes = "", childNode = text "☰ Flora"}

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
  let baseClasses = "navbar-link "
   in a_ [href_ href, class_ (baseClasses <> additionalClasses <> " " <> isActive isActive')] (text label)

navBarLink' :: Text -> Text -> Bool -> FloraHTML
navBarLink' = navBarLink ""

userMenu :: FloraHTML
userMenu = do
  ActiveElements{adminDashboard} <- asks activeElements
  TemplateEnv{mUser, sessionId} <- ask
  getUsernameOrLogin mUser
  adminLink adminDashboard mUser
  logOff mUser sessionId

navbarSearch :: FloraHTML
navbarSearch = do
  flag <- asks displayNavbarSearch
  if flag
    then do
      form_ [action_ "/search", method_ "GET"] $! do
        div_ [class_ "flex items-center py-2"] $! do
          label_ [for_ "search"] ""
          input_
            [ class_ "navbar-search"
            , id_ "search"
            , type_ "search"
            , name_ "q"
            , placeholder_ "Search a package"
            ]
    else pure mempty

logOff :: Maybe User -> PersistentSessionId -> FloraHTML
logOff Nothing _ = ""
logOff (Just _) sessionId =
  form_ [action_ ("/sessions/delete/" <> display sessionId), method_ "post", id_ "logoff"] $! do
    let btnClasses = "font-bold inline-flex items-center py-3 mx-4 text-white dark:text-gray-100 "
    button_ [type_ "submit", class_ btnClasses] "Sign out"

adminLink :: Bool -> Maybe User -> FloraHTML
adminLink active (Just user)
  | user.userFlags.isAdmin = navBarLink' "/admin" "Admin Dashboard" active
adminLink _ _ = ""

themeToggle :: FloraHTML
themeToggle = do
  let sunIcon = do
        img_ [src_ "/static/icons/sun.svg", class_ "h-6 w-6 invert"]

  let moonIcon = do
        img_ [src_ "/static/icons/moon.svg", class_ "h-6 w-6"]

  let buttonBaseClasses = "p-2 m-4 md:m-0 rounded-md inline-flex items-center bg-slate-200"

  button_
    [ xOn_ "click" "theme = 'light'; menuOpen = false"
    , class_ $! "theme-button--light " <> buttonBaseClasses
    ]
    sunIcon

  button_
    [ xOn_ "click" "theme = 'dark'; menuOpen = false"
    , class_ $! "theme-button--dark " <> buttonBaseClasses
    ]
    moonIcon

  input_ [type_ "checkbox", name_ "", id_ "darkmode-toggle", class_ "hidden", xModel_ [] "theme"]

getUsernameOrLogin :: Maybe User -> FloraHTML
getUsernameOrLogin Nothing = navBarLink' "/sessions/new" "Login/Signup" False
getUsernameOrLogin _ = "" -- navBarLink' "#" "Profile" False

isActive :: Bool -> Text
isActive True = " active"
isActive False = ""
