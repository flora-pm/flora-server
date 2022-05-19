module FloraWeb.Components.Footer where

import Lucid
import Lucid.Svg (clip_rule_, d_, fill_, fill_rule_, path_, viewBox_)

import FloraWeb.Templates.Types

footer :: FloraHTML
footer =
  footer_ [class_ "absolute inset-x-0 bottom-0 py-5"] $ do
    div_ [class_ "max-w-7xl mx-auto pt-12 px-4 sm:px-6 md:flex md:items-center md:justify-between lg:px-8"] $ do
      div_ [class_ "flex justify-center space-x-6 md:order-2"] $ do
        a_ [href_ "https://twitter.com/flora_haskell", class_ "text-gray-400 social-button"] $ do
          span_ [class_ "sr-only"] "Twitter"
          svg_ [class_ "h-6 w-6", fill_ "currentColor", viewBox_ "0 0 24 24"] $
            path_ [d_ "M8.29 20.251c7.547 0 11.675-6.253 11.675-11.675 0-.178 0-.355-.012-.53A8.348 8.348 0 0022 5.92a8.19 8.19 0 01-2.357.646 4.118 4.118 0 001.804-2.27 8.224 8.224 0 01-2.605.996 4.107 4.107 0 00-6.993 3.743 11.65 11.65 0 01-8.457-4.287 4.106 4.106 0 001.27 5.477A4.072 4.072 0 012.8 9.713v.052a4.105 4.105 0 003.292 4.022 4.095 4.095 0 01-1.853.07 4.108 4.108 0 003.834 2.85A8.233 8.233 0 012 18.407a11.616 11.616 0 006.29 1.84"]

        a_ [href_ "https://github.com/flora-pm", class_ "text-gray-400 social-button"] $ do
          span_ [class_ "sr-only"] "GitHub"
          svg_ [class_ "h-6 w-6", fill_ "currentColor", viewBox_ "0 0 24 24"] $
            path_ [fill_rule_ "evenodd", d_ "M12 2C6.477 2 2 6.484 2 12.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0112 6.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.202 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.943.359.309.678.92.678 1.855 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0022 12.017C22 6.484 17.522 2 12 2z", clip_rule_ "evenodd"]

      div_ [class_ "mt-8 md:mt-0 md:order-1"] $
        p_
          [class_ "text-center text-base text-black dark:text-gray-400"]
          "© 2022 Flora.pm. All rights reserved. Licensed under BSD-3-Clause."
