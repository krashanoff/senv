module Main where

import Senv

main :: IO ()
main = do
  input <- getContents
  print $ parseFile input
