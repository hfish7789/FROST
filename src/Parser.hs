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

-- | Module     : Parser
--   Copyright  : (c) XAIN Stichting, 2019
--   License    : GPL-3
--   Maintainer : info@xain.foundation
--   Stability  : experimental
--
-- Parser for FROST language syntax
--
module Parser
  ( frostParser,
    pol, decPol, rulePol, casePol,
    cExpr, cTerm,
    rExpr, relation,
    term
  ) where

-- useful MegaParsec reference:
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

import Lang
import Lexer
import Text.Megaparsec (between, eof, try, (<|>), (<?>), choice)
import Control.Monad.Combinators.Expr -- parser-combinators package
  (makeExprParser, Operator(..))


-- | Main parser of FROST policy expressions
--
-- takes care of initial whitespace
frostParser :: Parser Pol
frostParser = between sc eof pol
--
-- eof succeeds at end of input

-- | Policy parser
pol :: Parser Pol
pol = try rulePol
  <|> decPol
  <|> casePol
--
-- try parsing as rules before backtracking to decisions

-- | Constant policy parser
decPol :: Parser Pol
decPol = grant <$ word "grant"
     <|> deny  <$ word "deny"
     <|> confl <$ word "conflict"
     <|> gap   <$ word "undef"

-- | Rule policy parser
rulePol :: Parser Pol
rulePol = grantIf0 <$> (word "grant" *> word "if" *> cExpr)
      <|> denyIf0  <$> (word "deny"  *> word "if" *> cExpr)

-- | Case policy parser
--
-- NOTE not yet implemented
casePol :: Parser Pol
casePol = undefined

-- a NOTE on terminology below: generator elements are "terms". "expressions"
-- are built from terms and (typically unary or binary) operators. but
-- parenthesised expressions are also terms! (think of arithmetic / boolean
-- expressions rather than general algebra)

-- | Condition expression parser
cExpr :: Parser Cond
cExpr = makeExprParser cTerm cOps
  where
    cOps :: [[Operator Parser Cond]]
    cOps =  [[Prefix (Not <$ symbol "!")],  -- ! has highest precedence
             [InfixL (And <$ symbol "&&"),  -- && has same precedence...
              InfixL (Or  <$ symbol "||")]] -- as ||

-- | Condition term parser
cTerm :: Parser Cond
cTerm = parens cExpr
    <|> try rExpr
    <|> true  <$ word "true"
    <|> false <$ word "false"
--
-- better rExpr before true and false: an rExpr can start with true / false
-- try parsing them as terms before backtracking to conditions

-- | FROST term parser
--
-- NOTE attributed terms not yet supported
term :: Parser Term
term = subj <$  word "subject"
   <|> obj  <$  word "object"
   <|> act  <$  word "action"
   <|> name <$> identifier
   <|> LitI <$> integer
   <|> LitS <$> stringLit
   <|> LitB True  <$ word "true"
   <|> LitB False <$ word "false"

-- | Relational expression parser
rExpr :: Parser Cond
rExpr = do
  t1  <- term     <?> "left term"
  rel <- relation <?> "relation"
  t2  <- term     <?> "right term"
  return (rel t1 t2)
--
-- NOTE do-notation used because
--   relation <*> term <*> term
-- is not the right order (since relational op is infix)!
-- this would prob work:
-- (\t1 r t2 -> r t1 t2) <$> term <*> relation <*> term

-- | Relational predicate parser
relation :: Parser (Term -> Term -> Cond)
relation = choice -- alternative to chaining together with <|>
  [ (=:=) <$ symbol "=="
  , (>:=) <$ symbol ">="
  , (<:=) <$ symbol "<="
  , (<:)  <$ symbol "<"
  , (>:)  <$ symbol ">" ]
--
-- a Parser BinPred would maybe be a bit more robust but at the moment BinPred
-- is not exported from Lang (and arguably shouldn't need to) so we return the
-- smart constructors
