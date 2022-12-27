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
  Value String |
  Newline
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
  try (char '#')
  content <- manyTill anyChar ((toss endOfLine) <|> eof)
  return $ Comment content

-- |Identifies the assignment operator '='.
assignment :: Parsec String () Char
assignment = char '='

emptyValueNoQuote :: Parsec String () Char
emptyValueNoQuote = fmap (const '\'') endOfLine
emptySingle :: Parsec String () Char
emptySingle = char '\'' >> char '\''
emptyDouble :: Parsec String () Char
emptyDouble = char '"' >> char '"'

emptyValueStr :: Parsec String () String
emptyValueStr = do
  choice [emptySingle, emptyDouble, emptyValueNoQuote]
  return ""

unquoted :: Parsec String () Statement
unquoted = do
  Key val <- keyIdentifier
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
  $ [
    emptyValueStr
  , blockQuotedEscaped
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

toss :: Functor f0 => f0 a -> f0 ()
toss = fmap (const ())

tossEOL :: Parsec String () ()
tossEOL = toss endOfLine

statement :: Parsec String () Statement
statement = do
  optionalExport
  key <- keyIdentifier
  assignment
  val <- unquoted <|> someValue
  option () $ try optionalComment
  return (Assignment key val)

-- |Instead of Parsec's `spaces`, which includes EOL characters.
inlineWhitespace = many $ oneOf " \t"

emptyLine :: Parsec String () Statement
emptyLine = fmap (const Newline) $ (inlineWhitespace >> tossEOL)

validStatement :: Parsec String () Statement
validStatement = emptyLine <|> comment <|> statement

file :: Parsec String () [Statement]
file = do
  lines <- option [] $ many validStatement
  eof
  return lines

emptyFile :: Parsec String () [Statement]
emptyFile = do
  manyTill (spaces >> tossEOL) (try eof)
  return []

-- |Parse a .env file into something manageable.
parseEnv :: String -> Either ParseError [Statement]
parseEnv = parse (file <|> emptyFile) ""

-- |Parse a statement from an .env file. Potentially useful for testing.
parseStatement :: String -> Either ParseError Statement
parseStatement = parse validStatement ""
