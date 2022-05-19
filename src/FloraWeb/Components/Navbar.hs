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
          <> "bg-brand-purple-1 md:bg-brand-purple-1 dark:bg-dark-2 md:dark:bg-navbar-dark "
          <> "flex flex-col md:flex-row absolute md:relative top-[100%] left-0 w-full md:w-auto md:top-0"
  nav_ [class_ "sticky top-0 left-0 border-b dark:border-transparent bg-brand-purple-1 dark:bg-dark-2 mb-3 z-10", xData_ "{menuOpen: false}"] $ do
    div_ [id_ "navbar-content", class_ "max-w-9xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
      div_ [class_ "md:flex md:justify-between h-16 "] $ do
        div_ [id_ "navbar-left", class_ "flex flex-shrink-0"] $ do
          brand
          navbarSearch

        div_ [id_ "navbar-right", class_ menuClasses, xBind_ "class" "!menuOpen ? 'hidden' : ''"] $ do
          navBarLink "md:hidden " "/" title False
          navBarLink' "/about" "About Flora" aboutNav
          navBarLink' "/categories" "Categories" packagesNav
          navBarLink' "/packages" "Packages" packagesNav
          userMenu
          darkModeToggle

brand :: FloraHTML
brand = do
  TemplateEnv{title, mobileTitle} <- ask
  let link = a_ [href_ "/", id_ "brand", class_ "font-bold text-white dark:text-gray-100"]
  let containerBaseClasses = "flex items-center border-b-4 border-brand-purple-1 dark:border-brand-purple-1 flex-shrink-0 h-16"
  div_ [class_ $ containerBaseClasses <> " hidden md:flex"] $ link (text title)
  div_ [class_ $ containerBaseClasses <> " md:hidden", xOn_ "click.prevent" "menuOpen = !menuOpen"] $ link (text mobileTitle)

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
  TemplateEnv{mUser, sessionId} <- ask
  getUsernameOrLogin mUser
  adminLink mUser
  logOff mUser sessionId

navbarSearch :: FloraHTML
navbarSearch = do
  flag <- asks displayNavbarSearch
  if flag
    then do
      form_ [class_ "w-full max-w-sm ml-5 inline-flex", action_ "/search", method_ "GET"] $ do
        div_ [class_ "flex items-center py-2"] $ do
          input_
            [ class_ "rounded-full bg:bg-background dark:bg-background-dark-2 w-full mr-3 pl-3 py-1 px-1 leading-tight focus:outline-none border border-2 border-brand-purple"
            , id_ "packageName"
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

adminLink :: Maybe User -> FloraHTML
adminLink (Just user) | user ^. #userFlags ^. #isAdmin = li_ [class_ "user-menu-element"] $ a_ [href_ "/admin"] "Admin Dashboard"
adminLink _ = ""

darkModeToggle :: FloraHTML
darkModeToggle = do
  let lightModeContent = do
        img_ [src_ "/static/icons/moon.svg", class_ "h-6 w-6 mr-2"]
        "Dark Mode"
  let darkModeContent = do
        img_ [src_ "/static/icons/sun.svg", class_ "h-6 w-6 mr-2 invert"]
        "Light Mode"
  let buttonBaseClasses = "p-2 m-4 md:m-0 rounded-md inline-flex items-center bg-slate-200 dark:bg-brand-purple-1"
  button_ [xOn_ "click" "darkMode = !darkMode; menuOpen = false", class_ $ "hidden dark:inline-flex " <> buttonBaseClasses] darkModeContent
  button_ [xOn_ "click" "darkMode = !darkMode; menuOpen = false", class_ $ "dark:hidden " <> buttonBaseClasses] lightModeContent
  input_ [type_ "checkbox", name_ "", id_ "darkmode-toggle", class_ "hidden", xModel_ [] "darkMode"]

getUsernameOrLogin :: Maybe User -> FloraHTML
getUsernameOrLogin Nothing = navBarLink' "/sessions/new" "Login/Signup" False
getUsernameOrLogin _ = navBarLink' "#" "Profile" False

isActive :: Bool -> Text
isActive True = " active"
isActive False = ""
