module Senv (
    Statement (..)
  , parseStatement
  , parseEnv
) where

import Control.Monad (foldM)

import Text.Parsec

-- |AST for an env file.
data Statement =
  Identifier String |
  Comment String |
  Assignment Statement Statement |
  Key String |
  Value String
  deriving (Show, Eq)

-- |Initial character for some unquoted value or an identifier.
initialCharacter :: Parsec String () Char
initialCharacter = choice [(char '_'), letter]

-- |Valid character for any part of an unquoted value or an identifier.
validChar :: Parsec String () Char
validChar = choice [(char '_'), alphaNum]

-- |Escaped character sequence. EOF maps to null.
escapedChar :: Parsec String () Char
escapedChar = char '\\' >> choice [noneOf "\r\n\t", fmap (const '\0') eof]

-- |Identifies a key (the part to the left of the '=' symbol) or unquoted value.
keyIdentifier :: Parsec String () Statement
keyIdentifier = do
  first <- initialCharacter
  rest <- many validChar
  return $ Key (first : rest)

comment :: Parsec String () Statement
comment = do
  char '#'
  content <- manyTill anyChar (try (choice [toss endOfLine, eof]))
  return $ Comment content

-- |Identifies the assignment operator '='.
assignment :: Parsec String () Char
assignment = char '='

unquoted :: Parsec String () Statement
unquoted = do
  Key val <- keyIdentifier
  option () (toss $ try comment)
  (toss endOfLine) <|> eof
  return $ Value val

quotedBy :: Either Char String -> Parsec String () String
quotedBy (Left c) = do
  char c
  value <- manyTill anyChar (try (char c))
  return value
quotedBy (Right s) = do
  string (s ++ "\n")
  results <- manyTill anyChar (try (string s))
  return results

singleQuoted = quotedBy (Left '\'')
doubleQuoted = quotedBy (Left '"')
blockQuoted = quotedBy (Right "'''")
blockQuotedEscaped = quotedBy (Right "\"\"\"")

-- |Tries all the possible value parsers in order. Returns a string.
someValueStr :: Parsec String () String
someValueStr = option ("")
  (choice
  $ fmap (try) [
    blockQuotedEscaped
  , blockQuoted
  , doubleQuoted
  , singleQuoted
  ])

someValue :: Parsec String () Statement
someValue = fmap (Value) someValueStr

optionalComment :: Parsec String () ()
optionalComment = option () (toss (spaces >> comment))

optionalExport :: Parsec String () ()
optionalExport = option () (toss (string "export" >> spaces))

tossEOL :: Parsec String () ()
tossEOL = toss endOfLine

toss :: Functor f0 => f0 a -> f0 ()
toss = fmap (const ())

statement :: Parsec String () Statement
statement = do
  optionalExport
  key <- keyIdentifier
  assignment
  val <- someValue
  option () $ try optionalComment
  return (Assignment key val)

statementWithNewLine :: Parsec String () Statement
statementWithNewLine = do
  result <- statement
  choice [fmap (const ()) endOfLine, try eof]
  return result

validStatement :: Parsec String () (Maybe Statement)
validStatement =
  let emptyLine = spaces >> tossEOL in
  (fmap Just statementWithNewLine)
  <|> (fmap Just unquoted)
  <|> (fmap Just comment)
  <|> (fmap (const Nothing) emptyLine)

-- |A valid line in a .env file.
validLine :: Parsec String () (Maybe Statement)
validLine = do
  result <- try validStatement
  return result

foldMaybes :: [Maybe a] -> [a]
foldMaybes = foldl (\a b -> case b of
  Just d -> d : a
  Nothing -> a)
  []

file :: Parsec String () [Statement]
file = do
  lines <- option [] $ many validLine
  eof
  return $ foldMaybes lines

emptyFile :: Parsec String () [Statement]
emptyFile = do
  many (spaces >> tossEOL)
  eof
  return []

-- |Parse a .env file into something manageable.
parseEnv :: String -> Either ParseError [Statement]
parseEnv = parse (file <|> emptyFile) ""

-- |Parse a statement from an .env file. Potentially useful for testing.
parseStatement :: String -> Either ParseError Statement
parseStatement = parse statement ""
