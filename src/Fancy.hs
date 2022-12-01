module Fancy (envFileDef)
where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

envFileDef = LanguageDef
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = "#"
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opLetter emptyDef
               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }