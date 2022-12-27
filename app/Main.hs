module Main where

import Senv

displayAsFish :: [Statement] -> String
displayAsFish = foldl
  (\item acc -> case item of
    Assignment (Key k) (Value v) -> acc ++ "\nset -gx " ++ k ++ " " ++ v
    otherwise -> acc
  )
  ""

main :: IO ()
main = do
  parsed <- fmap parseEnv getContents
  display <- displayAsFish parsed
  case parsed of
    Right statements -> putStrLn display
    Left err -> putStrLn $ show err
