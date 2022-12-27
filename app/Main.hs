module Main where

import Senv

import Options.Applicative
import Data.Semigroup ((<>))

data ShellOpt = Fish
  deriving (Show, Read)

data Sample = Sample
  {
    shellOpt :: ShellOpt
  , exportAll :: Bool
  }

sample :: Parser Sample
sample = Sample
      <$> option auto
          ( long "target-shell"
         <> short 't'
         <> help "Shell to output for"
         <> showDefault
         <> value Fish
         <> metavar "SHELLNAME" )
      <*> switch
          ( long "export-all"
          <> short 'E'
          <> help "Export all variables" )

displayAsFish :: [Statement] -> String
displayAsFish = foldl
  (\acc item -> case item of
    Assignment (Key k) (Value v) -> acc ++ "\nset -gx " ++ k ++ " \"\"\"" ++ v ++ "\"\"\""
    otherwise -> acc
  )
  ""

operate :: Sample -> IO ()
operate (Sample shell _) = do
  parsed <- fmap parseEnv getContents
  case parsed of
    Right statements -> putStrLn $ displayAsFish statements
    Left err -> putStrLn $ show err

main :: IO ()
main = operate =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Convert a .env file to something your shell likes"
     <> header "senv - sane .env"
     <> footer "Bugs: https://github.com/krashanoff/senv" )
