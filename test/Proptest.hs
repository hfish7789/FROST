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

module Proptest where

import Test.QuickCheck
import DacSimple

instance Arbitrary Const where
  arbitrary = oneof (map return [Subj, Obj, Act])

instance Arbitrary BinPred where
  arbitrary = oneof (map return [Equ, Lt, Lte, Gte])

-- NOTE choose from a Keyword or Entity "var0".."var9"
--      ignore attributed terms
instance Arbitrary Term where
  arbitrary = frequency [(3, Keyword <$> arbitrary), (10, ent)]
    where
      ent = do digit <- elements [0..9]
               let name = 'v' : show digit
               return (Entity name)

instance Arbitrary Cond where
  arbitrary = sized condTree

condTree :: Int -> Gen Cond
condTree n
  | n == 0    = oneof [true, binRel]
  | otherwise = oneof [true, binRel,
                       Not <$> condTree (n-1),
                       And <$> subtree <*> subtree,
                       Or  <$> subtree <*> subtree]
  where
    true    = return T
    binRel  = BinRel <$> arbitrary <*> arbitrary <*> arbitrary
    subtree = condTree (n `div` 2)

instance Arbitrary Rule where
  arbitrary = oneof [GrantIf <$> arbitrary, DenyIf <$> arbitrary]

instance Arbitrary Dec where
  arbitrary = oneof (map return [Grant, Deny, Gap, Conflict])

instance Arbitrary Guard where
  arbitrary = sized guardTree

guardTree :: Int -> Gen Guard
guardTree n
  | n == 0    = oneof [truth, eval]
  | otherwise = oneof [truth, eval, Conj <$> subtree <*> subtree]
  where
    truth   = return Truth
    eval    = Eval <$> polTree (n `div` 2) <*> arbitrary
    subtree = guardTree (n `div` 2)

instance Arbitrary Pol where
  arbitrary = sized polTree

polTree :: Int -> Gen Pol
polTree n
  | n == 0    = oneof [konst, rule]
  | otherwise = oneof [konst, rule, Case <$> arms <*> subtree]
  where
    konst   = Konst  <$> arbitrary
    rule    = Filter <$> arbitrary
    subtree = polTree (n `div` 2)
    arms    = listOf $ (,) <$> guardTree (n `div` 2) <*> subtree -- listOf knows about n?

instance Arbitrary Env where
  arbitrary = do
    sVal <- choose (0, 9)
    oVal <- choose (0, 9)
    aVal <- choose (0, 9)
    let sPair = (Keyword Subj, sVal)
    let oPair = (Keyword Obj,  oVal)
    let aPair = (Keyword Act,  aVal)
    eVals <- vectorOf 10 (choose (0, 9))
    let eKeys = map Entity ['v' : show d | d <- [0..9]]
    let ePairs = zip eKeys eVals
    return $ Env $ sPair : oPair : aPair : ePairs

prop_commJoin :: Pol -> Pol -> Env -> Bool
prop_commJoin p q env = evalP (p `join` q) env == evalP (q `join` p) env

prop_idemJoin :: Pol -> Env -> Bool
prop_idemJoin p env = evalP p env == evalP (p `join` p) env

prop_assocJoin :: Pol -> Pol -> Pol -> Env -> Bool
prop_assocJoin p q r env =
  evalP ((p `join` q) `join` r) env == evalP (p `join` (q `join` r)) env

prop_ruleNoConflict :: Rule -> Env -> Bool
prop_ruleNoConflict r env = not $ evalG (Filter r `Eval` Conflict) env

prop_circuitPair :: Pol -> Env -> Bool
prop_circuitPair p env = case (evalC (goc p) env,
                               evalC (doc p) env) of
  (True, True)   -> evalG (p `Eval` Conflict) env
  (True, False)  -> evalG (p `Eval` Grant) env
  (False, True)  -> evalG (p `Eval` Deny) env
  (False, False) -> evalG (p `Eval` Gap) env

prop_joinNormalForm :: Pol -> Env -> Bool
prop_joinNormalForm p env =
  evalP p env == evalP (grantIf (goc p) `join` denyIf (doc p)) env

prop_compositionalJoin :: Pol -> Pol -> Env -> Bool
prop_compositionalJoin p q env =
  evalP (p `join` q) env == evalP p env `joinK` evalP q env

-- NOTE use of break is quite neat but requires Guard to be Eq
-- which in turn requires Pol, etc. leave out for now...
-- prop_defaultGuard :: [(Guard, Pol)] -> Pol -> Env -> Bool
-- prop_defaultGuard arms def env =
--   evalP (Case arms def) env == evalP (Case arms' def') env
--   where
--     (arms', (Truth, def'):_) = break ((== Truth) . fst) (arms ++ [(Truth, def)])
