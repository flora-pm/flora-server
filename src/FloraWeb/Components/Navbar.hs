module FloraWeb.Components.Navbar where

import Control.Monad.Reader (ask, asks)
import Data.Text (Text)
import Data.Text.Display (display)
import Lucid
import Lucid.Alpine
import Optics.Core

import Flora.Model.PersistentSession (PersistentSessionId (..))
import Flora.Model.User (User (..), UserFlags (..))
import FloraWeb.Components.Utils
import FloraWeb.Templates.Types

navbar :: FloraHTML
navbar = do
  ActiveElements{aboutNav, packagesNav} <- asks activeElements
  TemplateEnv{title} <- ask
  let menuClasses =
        "md:flex flex md:items-center "
          <> "bg-purple-1 md:bg-purple-1 dark:bg-blue-1 md:dark:bg-navbar-dark "
          <> "flex flex-col md:flex-row absolute md:relative top-[100%] left-0 w-full md:w-auto md:top-0"
  nav_ [class_ "sticky top-0 left-0 border-b dark:border-transparent bg-purple-1 dark:bg-blue-1 mb-3 z-10", xData_ "{menuOpen: false}"] $ do
    div_ [id_ "navbar-content", class_ "max-w-9xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
      div_ [class_ "md:flex md:justify-between h-16 "] $ do
        div_ [id_ "navbar-left", class_ "flex flex-shrink-0"] $ do
          brand
          navbarSearch

        div_ [id_ "navbar-right", class_ menuClasses, xBind_ "class" "!menuOpen ? 'hidden' : ''"] $ do
          navBarLink "md:hidden " "/" title False
          navBarLink' "/about" "About" aboutNav
          navBarLink' "/categories" "Categories" packagesNav
          navBarLink' "/packages" "Packages" packagesNav
          userMenu
          darkModeToggle

brand :: FloraHTML
brand = do
  TemplateEnv{title, mobileTitle} <- ask
  let containerBaseClasses = "flex items-center flex-shrink-0 h-16"
  div_ [class_ $ containerBaseClasses <> " hidden md:flex"] $
    div_ [class_ "brand"] $
      link defaultLinkOptions{href = "/", classes = "font-bold text-white dark:text-gray-100", childNode = text title}
  div_ [class_ $ containerBaseClasses <> " md:hidden", xOn_ "click.prevent" "menuOpen = !menuOpen"] $
    link defaultLinkOptions{href = "/", classes = "font-bold text-white dark:text-gray-100", childNode = text mobileTitle}

navBarLink ::
  -- | Additional classes
  Text ->
  -- | href attribute
  Text ->
  -- | label
  Text ->
  -- | is the element active
  Bool ->
  FloraHTML
navBarLink additionalClasses href label isActive' =
  let baseClasses = "font-bold inline-flex items-center py-3 mx-4 text-white dark:text-gray-100 "
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
      form_ [class_ "w-full max-w-sm ml-5 inline-flex", action_ "/search", method_ "GET"] $ do
        div_ [class_ "flex items-center py-2"] $ do
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
  form_ [action_ ("/sessions/delete/" <> display sessionId), method_ "post", id_ "logoff"] $ do
    let btnClasses = "font-bold inline-flex items-center py-3 mx-4 text-white dark:text-gray-100 "
    button_ [type_ "submit", class_ btnClasses] "Sign out"

adminLink :: Bool -> Maybe User -> FloraHTML
adminLink active (Just user) | user ^. #userFlags ^. #isAdmin = navBarLink' "/admin" "Admin Dashboard" active
adminLink _ _ = ""

darkModeToggle :: FloraHTML
darkModeToggle = do
  let lightModeContent = do
        img_ [src_ "/static/icons/moon.svg", class_ "h-6 w-6 mr-2"]
        "Dark Mode"
  let darkModeContent = do
        img_ [src_ "/static/icons/sun.svg", class_ "h-6 w-6 mr-2 invert"]
        "Light Mode"
  let buttonBaseClasses = "p-2 m-4 md:m-0 rounded-md inline-flex items-center bg-slate-200 dark:bg-purple-3"
  button_ [xOn_ "click" "darkMode = !darkMode; menuOpen = false",
           class_ $ "hidden dark:inline-flex " <> buttonBaseClasses] darkModeContent
  button_ [xOn_ "click" "darkMode = !darkMode; menuOpen = false",
           class_ $ "dark:hidden " <> buttonBaseClasses] lightModeContent
  input_ [type_ "checkbox", name_ "", id_ "darkmode-toggle", class_ "hidden", xModel_ [] "darkMode"]

getUsernameOrLogin :: Maybe User -> FloraHTML
getUsernameOrLogin Nothing = navBarLink' "/sessions/new" "Login/Signup" False
getUsernameOrLogin _ = ""--navBarLink' "#" "Profile" False

isActive :: Bool -> Text
isActive True = " active"
isActive False = ""
