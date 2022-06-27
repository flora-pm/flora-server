module Main where

import FloraWeb.Server
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  runFlora
