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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

module AttrLang where
import Data.Typeable
import Data.Dynamic
import Data.Maybe (fromMaybe)

data Const = Subj | Obj | Act deriving Eq

newtype Attrib a = Attrib String
-- NOTE might want to limit the types of attributes via GADT

data Term a where
  Entity :: String -> Term String -- "base" term
  Const  :: Const -> Term Const   -- ditto
  Attr   :: Term a -> Attrib b -> Environ b -> Term b

instance Eq (Term a) where
  (==) = heq

heq :: Term a -> Term b -> Bool
Entity s `heq` Entity s' = s == s'
Const c `heq` Const c' = c == c'
Attr t (Attrib a1) _ `heq` Attr t' (Attrib a2) _ = a1 == a2 && t `heq` t'
_ `heq` _ = False

data CompTerm a where
  Simple :: Term a -> CompTerm a
  UnOp   :: UnOp a b -> CompTerm a -> CompTerm b
  BinOp  :: BinOp a b c -> CompTerm a -> CompTerm b -> CompTerm c
  -- TernOp etc.

data UnOp a b where
  Negative :: UnOp Int Int
  Print :: Show a => UnOp a String
  -- etc.

data BinOp a b c where
  Add :: BinOp Int Int Int

data Cond where
  UnRel :: UnRel a -> Term a -> Cond
  BinRel :: BinRel a b -> Term a -> Term b -> Cond
  T :: Cond
  Not :: Cond -> Cond
  And :: Cond -> Cond -> Cond
  Or :: Cond -> Cond -> Cond

data UnRel a where -- a is the type of its argument
  IsTrue :: UnRel Bool
  IsZero :: UnRel Int

data BinRel a b where
  Equ :: Eq a => BinRel a a
  Lt :: Ord a => BinRel a a

type Environ a = [(Term a, a)]

evalT :: Term a -> a
evalT (Entity name) = name
evalT (Const const) = const
evalT at@(Attr _ _ e) =
  fromMaybe (error "Attribute not found") (lookup at e)

evalCT :: CompTerm a -> a
evalCT (Simple t) = evalT t
evalCT (UnOp Negative ct) = - (evalCT ct)
evalCT (UnOp Print ct) = show (evalCT ct)
evalCT (BinOp Add ct1 ct2) = evalCT ct1 + evalCT ct2

evalB :: Cond -> Bool
evalB T = True
evalB (Not c) = not (evalB c)
evalB (And c1 c2) = evalB c1 && evalB c2
evalB (Or c1 c2) = evalB c1 || evalB c2
evalB (UnRel IsTrue t) = evalT t
evalB (UnRel IsZero t) = evalT t == 0
evalB (BinRel Equ t1 t2) = evalT t1 == evalT t2
evalB (BinRel Lt t1 t2) = evalT t1 < evalT t2

st1 = [(Entity "vehicle" <@> "owner" <#> "age", 18),
       (Entity "vehicle" <#> "mileage", 10000)]

st2 = [(Entity "vehicle" <@> "owner", "john smith"),
       (Entity "vehicle" <#> "mileage" <@> "unit", "miles")]

infixl 8 <#>
(<#>) :: Term a -> String -> Term Int
t <#> name = Attr t (Attrib name) st1

infixl 8 <@>
(<@>) :: Term a -> String -> Term String
t <@> name = Attr t (Attrib name) st2

-- APPENDIX

data AttrVal = StringVal String | IntVal Int | CharVal Char
data AttrTree = Node String AttrVal [AttrTree]

attrStore :: AttrTree
attrStore = Node "vehicle" (StringVal "Ford") []

data Env' where
  MkEnv :: (UnRel a -> a -> Bool)
        -> (BinRel a b -> a -> b -> Bool)
        -> Env'

rho1 :: UnRel a -> a -> Bool
rho1 IsTrue b = b

rho2 :: BinRel a b -> a -> b -> Bool
rho2 Equ x y = x == y
rho2 Lt  x y = x < y

rho :: Env'
rho = MkEnv rho1 rho2

data Env
-- do we even need an env now? easy to argue against
-- since we don't have variables to update
-- Env' doesn't work sadly, but it's contrived anyway


data BinTree a where
  NodeS :: String -> String -> BinTree a -> BinTree b -> BinTree String
  NodeI :: String -> Int -> BinTree a -> BinTree b -> BinTree Int
  NodeC :: String -> Char -> BinTree a -> BinTree b -> BinTree Char
  Leaf  :: BinTree a

tree = NodeI "root" 3 (NodeC "left" 'y' Leaf Leaf)  (NodeC "right" 'c' Leaf Leaf)

age :: Attrib Int
age = Attrib "age"

-- attrPairs = [(Entity "vehicle", "Lamborghini"),
--             (Attr (Entity "vehicle") owner, 1234)]

owner :: Attrib String
owner = Attrib "owner"

mileage :: Attrib Int
mileage = Attrib "mileage"

data Showable a where
  Pack :: Show a => a -> Showable a

store :: [([String], Dynamic)]
store = [(["vehicle"], toDyn "Lambo"),
         (["vehicle", "owner"], toDyn "john smith"),
         (["vehicle", "owner", "age"], toDyn (50 :: Int))]
