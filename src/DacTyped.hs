{- This file is part of the Haskell version of the FROST compiler.

   Copyright (C) 2018  XAIN Stichting, Amsterdam  info@xain.foundation

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

{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}

module DacTyped where

data Const = Subj | Obj | Act deriving Eq

data Term a where
  EntityS  :: String -> Term String
  EntityI  :: String -> Term Integer
  EntityV  :: String -> Term Vehicle
  KeywordS :: Const  -> Term String
  KeywordI :: Const  -> Term Integer
  Attrib   :: Term a -> Path a b -> Term b

newtype Path a b = Path (a -> b) -- -> type is too general, really

data Vehicle = Vehicle { owner   :: Person,
                         reg     :: String,
                         mileage :: Integer }

data Person = Person { age  :: Integer,
                       name :: String }

driverAge :: Term Integer
driverAge = EntityV "v1" <@> owner <@> age -- or: age . owner

infixl 8 <@>
(<@>) :: Term a -> (a -> b) -> Term b
(<@>) t = Attrib t . Path

data UnPred a where -- a is the type of its argument
  IsTrue :: UnPred Bool
  IsZero :: UnPred Integer

data BinPred a b where
  Equ    :: Eq a  => BinPred a a
  Lt     :: Ord a => BinPred a a
  Lte    :: Ord a => BinPred a a
  Gt     :: Ord a => BinPred a a
  Member :: Eq a  => BinPred a [a]

data Cond where
  UnRel  :: UnPred a    -> Term a           -> Cond
  BinRel :: BinPred a b -> Term a -> Term b -> Cond
  T      :: Cond
  Not    :: Cond -> Cond
  And    :: Cond -> Cond -> Cond
  Or     :: Cond -> Cond -> Cond

-- compiling to circuits should be no different

-- TODO env, lookup, evalC

-- the question is how to emit the typing metadata in the json
