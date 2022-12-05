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
  InterpolatedValue String |
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
  string "# "
  content <- manyTill anyChar (try (choice [toss endOfLine, eof]))
  return $ Comment content

-- |Identifies the assignment operator '='.
assignment :: Parsec String () Char
assignment = char '='

unquoted :: Parsec String () String
unquoted = do
  Key val <- keyIdentifier
  try comment
  return val

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
    unquoted
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
  endOfLine
  return result

file :: Parsec String () [Statement]
file = do
    firstStatements <- option [] (many statementWithNewLine)
    optionalLast <- option Nothing (fmap (Just) (try $ choice [comment, statement]))
    many (choice [spaces, tossEOL])
    eof
    return $ case optionalLast of
      Just l -> firstStatements ++ [l]
      Nothing -> firstStatements

-- |Parse a .env file into something manageable.
parseEnv :: String -> Either ParseError [Statement]
parseEnv = parse file ""

-- |Parse a statement from an .env file. Potentially useful for testing.
parseStatement :: String -> Either ParseError Statement
parseStatement = parse statement ""
