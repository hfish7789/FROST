{- This file is part of the Haskell version of the FROST compiler.

   Copyright (C) 2019  XAIN Stichting, Amsterdam  info@xain.foundation

   The Haskell version of the FROST compiler is free software: you can
   redistribute it and/or modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   The Haskell version of the FROST compiler is distributed in the hope that it
   will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -Wall #-}

-- | Module     : Lexer
--   Copyright  : (c) XAIN Stichting, 2019
--   License    : GPL-3
--   Maintainer : info@xain.foundation
--   Stability  : experimental
--
-- Lexer for FROST language syntax
--
module Lexer
  ( Parser,
    sc, lexeme, symbol,
    parens, brackets,
    integer,
    semi, comma, colon, dot,
    word, identifier, rws, stringLit
  ) where

-- useful Megaparsec reference:
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

import Data.Void
import Text.Megaparsec
  (Parsec, empty, between, try, notFollowedBy, many, manyTill)
import Text.Megaparsec.Char (space1, string, alphaNumChar, letterChar, char)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type based on standard strings for now
type Parser = Parsec Void String

-- | space consumer
sc :: Parser ()
sc = L.space space1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "--"

-- | lexeme wrapper
--
-- consumes all whitespace after the lexeme (but not before it)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | fixed-string parser
symbol :: String -> Parser String
symbol = L.symbol sc

parens, brackets :: Parser a -> Parser a
-- | Parentheses parser
parens   = between (symbol "(") (symbol ")")
-- | Square brackets parser
brackets = between (symbol "[") (symbol "]")

-- | Integer parser
integer :: Parser Integer
integer = lexeme L.decimal

semi, comma, colon, dot :: Parser String
-- | Semi-colon parser
semi  = symbol ";"
-- | Comma parser
comma = symbol ","
-- | Colon parser
colon = symbol ":"
-- | Dot parser
dot   = symbol "."

-- | word parser
--
-- checks that the word is not a prefix of a longer word
word :: String -> Parser ()
word w = lexeme $ try $ string w *> notFollowedBy alphaNumChar
--
-- try p adds backtracking to p
-- notFollowedBy p only succeeds when p fails
-- *> discards the value coming out of string w and keeps just the other one ()

-- | Identifier parser
identifier :: Parser String
identifier = lexeme $ try $ p >>= check
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x
--
-- p may parse identifiers that are keywords - don't want that!
-- hence the check (errors if it is ^)

-- | List of reserved words
rws :: [String]
rws = ["grant", "deny", "conflict", "undef", "if", "case", "eval", "true",
       "false", "subject", "object", "action"]

-- | Parser for double-quoted string literal
stringLit :: Parser String
stringLit = lexeme $ char '"' *> manyTill L.charLiteral (char '"')
-- https://markkarpov.com/megaparsec/megaparsec.html#char-and-string-literals
-- but NOTE that he didn't wrap with lexeme!
