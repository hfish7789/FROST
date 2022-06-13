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

{- |
Module      : DacSimple
Description : (Ignore for now)
Copyright   : (c) XAIN Stichting, 2018
License     : GPL-3
Maintainer  : info@xain.foundation
Stability   : experimental
-}
module DacSimple where

import Data.List (inits)
import Data.Maybe (fromMaybe)

data Const = Subj -- ^ Subject
           | Obj  -- ^ Object
           | Act  -- ^ Action
             deriving (Eq, Show)

data Term = Entity String
          | Attr Term String
          | Keyword Const
            deriving (Show, Eq)

data BinPred = Equ
             | Lt
             | Lte
             | Gte
               deriving Show

data Cond = BinRel BinPred Term Term
          | T
          | Not Cond
          | And Cond Cond
          | Or Cond Cond
            deriving Show

false = Not T

data Dec = Grant    -- ^ The top element in the truth ordering
         | Deny     -- ^ The bottom element in the truth ordering
         | Gap      -- ^ The bottom element in the knowledge ordering
         | Conflict -- ^ The top element in the knowledge ordering
           deriving (Eq, Show)

data Rule = GrantIf Cond
          | DenyIf Cond
            deriving Show

data Guard = Truth
           | Eval Pol Dec
           | Conj Guard Guard
             deriving (Show)

data Pol = Konst Dec
         | Filter Rule
         | Case [(Guard, Pol)] Pol
           deriving Show

infixl 8 <@>
(<@>) :: Term -> String -> Term
(<@>) = Attr

infixr 6 .&.
(.&.) :: Guard -> Guard -> Guard
(.&.) = Conj

grant, deny, gap, confl :: Pol
grant = Konst Grant
deny  = Konst Deny
gap   = Konst Gap
confl = Konst Conflict

grantIf, denyIf :: Cond -> Pol
grantIf = Filter . GrantIf
denyIf  = Filter . DenyIf

subj = Keyword Subj
obj  = Keyword Obj
act  = Keyword Act

-- | Takes two 'Pol'icies and merges their information content.
-- This is the analogous operation to 'joinK' lifted to the 'Pol' level.
join :: Pol -> Pol -> Pol
join p q = Case [(p `Eval` Gap,      q),
                 (q `Eval` Gap,      p),
                 (p `Eval` Conflict, confl),
                 (q `Eval` Conflict, confl),
                 (p `Eval` Grant .&.
                  q `Eval` Deny,     confl),
                 (q `Eval` Grant .&.
                  p `Eval` Deny,     confl)]
                                     p

newtype Env = Env { unEnv :: [(Term, Integer)] }
              deriving Show

-- | Evaluator for 'Term's
evalT :: Term -> Env -> Integer
evalT t = fromMaybe (-1) . lookup t . unEnv

-- | Evaluator for 'Cond'itions
evalC :: Cond -> Env -> Bool
evalC T _ = True
evalC (Not c) env            = not (evalC c env)
evalC (And c1 c2) env        = evalC c1 env && evalC c2 env
evalC (Or c1 c2) env         = evalC c1 env || evalC c2 env
evalC (BinRel Equ t1 t2) env = evalT t1 env == evalT t2 env
evalC (BinRel Lt t1 t2) env  = evalT t1 env <  evalT t2 env
evalC (BinRel Lte t1 t2) env = evalT t1 env <= evalT t2 env
evalC (BinRel Gte t1 t2) env = evalT t1 env >  evalT t2 env

-- | Evaluator for 'Guard's
evalG :: Guard -> Env -> Bool
evalG Truth _            = True
evalG (Eval pol dec) env = evalP pol env == dec
evalG (Conj g1 g2) env   = evalG g1 env && evalG g2 env

-- | Evaluator for 'Pol'icies
evalP :: Pol -> Env -> Dec
evalP (Konst dec) _                   = dec
evalP (Filter (GrantIf c)) env        = if evalC c env then Grant else Gap
evalP (Filter (DenyIf c)) env         = if evalC c env then Deny else Gap
evalP (Case [] defPol) env            = evalP defPol env
evalP (Case ((g, p) : as) defPol) env =
  evalP (if evalG g env then p else Case as defPol) env

-- | Translates a 'Pol'icy into a grant-or-conflict circuit
goc :: Pol -> Cond
goc (Konst Grant)           = T
goc (Konst Conflict)        = T
goc (Konst _)               = false
goc (Filter (GrantIf cond)) = cond
goc (Filter (DenyIf _))     = false
goc (Case [] defPol)        = goc defPol
goc (Case arms defPol)      = compCase True arms defPol

-- | Translates a 'Pol'icy into a deny-or-conflict circuit
doc :: Pol -> Cond
doc (Konst Deny)           = T
doc (Konst Conflict)       = T
doc (Konst _)              = false
doc (Filter (GrantIf _))   = false
doc (Filter (DenyIf cond)) = cond
doc (Case [] defPol)       = doc defPol
doc (Case arms defPol)     = compCase False arms defPol

t :: Guard -> Cond
t Truth = T
t (Eval pol Conflict) = goc pol `And` doc pol
t (Eval pol Gap)      = Not (goc pol) `And` Not (doc pol)
t (Eval pol Grant)    = goc pol `And` Not (doc pol)
t (Eval pol Deny)     = Not (goc pol) `And` doc pol
t (Conj g1 g2)        = t g1 `And` t g2

-- assume nonempty; none of the guards hold except the last
disjunct :: Bool -> [(Guard, Pol)] -> Cond
disjunct b arms = foldr (And . Not . t . fst) (t trueGuard) pairs
                  `And` compPol pol
  where
    (pairs, [(trueGuard, pol)]) = splitAt (length arms - 1) arms
    compPol = if b then goc else doc

lastDisjunct :: Bool -> [Guard] -> Pol -> Cond
lastDisjunct b gs pol = foldr (And . Not . t) T gs
                        `And` compPol pol
  where
    compPol = if b then goc else doc

compCase :: Bool -> [(Guard, Pol)] -> Pol -> Cond
compCase isGrant arms defPol =
  foldr (Or . disjunct isGrant) (lastDisjunct isGrant guards defPol) armInits
  where
    armInits = tail (inits arms)
    guards   = map fst arms

-- | Knowledge-join operation of the Belnap bilattice
joinK :: Dec -> Dec -> Dec
joinK Gap d = d
joinK d Gap = d
joinK Conflict _ = Conflict
joinK _ Conflict = Conflict
joinK Grant Deny = Conflict
joinK Deny Grant = Conflict
joinK d _        = d -- Grant, Grant; Deny, Deny
