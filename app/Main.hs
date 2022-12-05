module Main where

import Senv

main :: IO ()
main = do
  parsed <- fmap parseEnv getContents
  putStrLn $ show parsed
