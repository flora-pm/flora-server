module FloraWeb.API where

data Routes mode = Routes
  { assets :: mode :- "static" :> Raw
  , pages  :: mode :- AuthProtect "cookie-auth" :> Pages.Routes
  }
  deriving stock (Generic)
