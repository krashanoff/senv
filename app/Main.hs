module Main where

import MyLib

main :: IO ()
main = do
  input <- getContents
  print $ parseFile input
