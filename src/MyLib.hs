module MyLib (
    Statement
  , parseStatement
) where

import Text.Parsec

someFunc = putStrLn "someFunc"

-- |AST for an env file.
data Statement =
  Identifier String |
  Comment String |
  Key String |
  InterpolatedValue String |
  Value [Either String Statement]
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

-- |Identifies a key (the part to the left of the '=' symbol).
keyIdentifier :: Parsec String () String
keyIdentifier = initialCharacter >> many validChar

comment :: Parsec String () ()
comment = char '#' >> many anyChar >> (option () (fmap (const ()) endOfLine))

-- |Identifies the assignment operator '='.
assignment = char '='

unquoted :: Parsec String () String
unquoted = keyIdentifier >> (option "" (fmap (const "") (spaces >> comment)))

singleQuoted :: Parsec String () Char
singleQuoted = char '\'' >> many anyChar >> char '\''

doubleQuoted :: Parsec String () Char
doubleQuoted = char '"' >> many anyChar >> char '"'

blockQuoted :: Parsec String () String
blockQuoted = string "'''" >> many anyChar >> string "'''"

blockQuotedEscaped :: Parsec String () String
blockQuotedEscaped = string "\"\"\"" >> many anyChar >> string "\"\"\""

someValue :: Parsec String () String
someValue = choice [
    unquoted
  , blockQuotedEscaped
  , blockQuoted
  , fmap (:[]) doubleQuoted
  , fmap (:[]) singleQuoted]

ignoreComment :: Parsec String () ()
ignoreComment = option () (fmap (const ()) (spaces >> comment))

optionalExport = option () (fmap (const ()) (string "export" >> spaces))

statement :: Parsec String () ()
statement = optionalExport >> keyIdentifier >> assignment >> someValue >> (option () ignoreComment)

file = (many statement >> eof) <|> eof

parseFile = parse file ""

parseStatement = parse statement ""
