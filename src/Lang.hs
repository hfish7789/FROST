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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      : Lang
Copyright   : (c) XAIN Stichting, 2019
License     : GPL-3
Maintainer  : info@xain.foundation
Stability   : experimental

Standard library of syntactic constructs for the authoring of FROST policies
-}
module Lang
  ( -- * Decisions
    --
    -- $decisions
    Dec(..),
    -- $lattices
    joinK, meetK, negK, joinT, meetT, negT,
    -- * Conditions
    --
    -- $conditions
    Cond(T, And, Or, Not),
    true, false, (&:&), (|:|), (¬),
    (=:=), (>:=), (<:=), (<:), (>:),
    Term(LitI, LitB, LitS),
    subj, obj, act,
    name, (<@>),
    -- * Policies
    --
    -- $policies
    Pol(Case),
    denyByDefault, join, neg, (>:>),
    grantIf, denyIf, grantIf0, denyIf0, ifTarget,
    grant, deny, gap, confl,
    Guard,
    (.&.), (~~>),
    Oblg(..),
    Env, evalP0, evalP,
    -- * Compilation
    --
    -- $compilation
    Circuit(..),
    compile0, goc, doc,
    penc, encodeFilePretty,
    -- | == Re-exports
    -- Exported from @aeson@ and @aeson-pretty@ packages
    encode, encodeFile,
    encodePretty
  ) where

import Data.List (inits)
import Control.Monad.Writer.Lazy hiding (join)
import Data.Maybe (fromMaybe, isJust, fromJust)
-- TODO factor json rendering out to separate module
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import Data.Aeson.Encode.Pretty (encodePretty)
-- we use Lazy variants since encode(Pretty) returns lazy bytestring...
import qualified Data.Text.Lazy.IO as TIO      -- putStrLn
import qualified Data.Text.Lazy.Encoding as TE -- decodeUtf8
import qualified Data.Char as C                -- toLower
import qualified Data.ByteString.Lazy as B     -- writeFile


data Const = Subj | Obj | Act deriving (Eq, Show)

{- | FROST terms are either basic or attributed. A basic term is one of the
constants

* 'subj'
* 'obj'
* 'act'

or a literal value (either an integer, boolean or string) e.g.

@
LitI 800
LitB True
LitS "john"
@

or a named entity e.g.

@
'name' "entity"
@

Attributed terms are of the form

@
t '<@>' "attribute"
@

where @t@ is a (non-literal, possibly attributed) 'Term'
-}
data Term = LitI Integer
          | LitB Bool
          | LitS String
          | ATerm ATerm

data ATerm = Entity String
           | Attr ATerm String
           | Keyword Const
             deriving Eq

instance Show ATerm where
  show (Entity name)  = name
  show (Keyword Subj) = "subject"
  show (Keyword Obj)  = "object"
  show (Keyword Act)  = "action"
  show (Attr t attr)  = show t ++ ('.' : attr)

instance Show Term where
  show (LitI n)     = show n
  show (LitB True)  = "true"
  show (LitB False) = "false"
  show (LitS s)     = s
  show (ATerm t)    = show t

-- | @t \<\@\> attr@ constructs an attributed term with the accessor name @attr@
-- on the base term @t@
--
-- /Note: undefined if the base term/ @t@ /is a literal/
(<@>) :: Term -> String -> Term
ATerm t <@> attr = ATerm (Attr t attr)
-- TODO maybe handle _ case with more informative error msg?

data BinPred = Equ | Lt | Lte | Gt | Gte -- etc.
               deriving (Show, Generic)

{- $conditions

A /rule/ in FROST determines the conditions under which an access request is
granted or denied. These conditions are specified as values of type 'Cond' -
propositional formulae possibly containing predicate relations over FROST
'Term's. For example,

@
('subj' '=:=' 'name' "driver")  '&:&'
('obj'  '=:=' 'name' "vehicle") '&:&'
('act'  '=:=' 'name' "locate"   '|:|' ('act'         '=:=' 'name' "unlock"
                          '&:&'  'name' "time" '<:=' 'name' "deadline"))
@

In the above, the operators '&:&' and '|:|' of the 'Cond' type play a similar
role to the connectives '&&' and '||' (respectively) of the standard 'Bool'
type. The atomic conditions inbetween are constructed with relational operators
such as 'Term' equality '=:=' and inequality e.g. '<:='.

In fact, conditions form a lower-level language that /any/ 'Pol'icy can be
translated into (see 'Compilation'). Policies in this form lend themselves to
analysis and verification with other tooling such as SMT solvers.
-}

-- TODO consider smart constructors and hide BinRel
-- | A propositional logic whose atomic formulae consist of predicate relations
-- on 'Term's
data Cond = BinRel BinPred Term Term
            -- ^ /Note: use operators to construct relations, e.g./ '=:='
          | T             -- ^ /Also see/ 'true'
          | Not Cond      -- ^ /Also see/ '¬'
          | And Cond Cond -- ^ /Also see/ '&:&'
          | Or Cond Cond  -- ^ /Also see/ '|:|'
            deriving Show

-- | Obligations to be enacted alongside an access control decision
data Oblg = Log Int | NotifyUser String deriving Show
-- TODO perhaps just make this alias for String

{- $decisions

Values of the 'Dec' type should be thought of as access control decisions. The
values 'Grant' and 'Deny' have the intended meaning, and we call these
/definite/ decisions. On the other hand, the decision 'Gap' represents a lack of
information to conclude either of the two, while 'Conflict' represents an
excess of such information.

These four values live in the /Belnap (bi-)lattice/ (see e.g.
<http://doi.acm.org/10.1145/1952982.1952991>):

@
Knowledge
    ^
    |           'Conflict'
    |            ..*..
    |           \/     \\
    |          \/       \\
    |         \/         \\
    |  'Deny' *:           :* 'Grant'
    |         \\         \/
    |          \\       \/
    |           \\     \/
    |            ..*..
    |             'Gap'
    +---------------------------- > Truth
@

* 'Grant' and 'Deny' are the respective top and bottom elements of the lattice
  in the @Truth@ ordering

* 'Conflict' and 'Gap' are the respective top and bottom elements of the lattice
  in the @Knowledge@ ordering

-}

-- | Access control decision values in the Belnap lattice
data Dec = Grant
         | Deny
         | Gap
         | Conflict
           deriving (Eq, Show)

{- $lattices
=== Lattice Structure
The 'Dec' type is equipped with the mathematical
structure of bi-lattices, such as a @join@ and @meet@ for both @Truth@ and
@Knowledge@ orderings. These satisfy the standard lattice axioms, e.g. the
absorption laws:

@
-- absorption law 1
d \``joinK`\` (d \``meetK`\` d') = d

-- absorption law 2
d \``meetK`\` (d \``joinK`\` d') = d
@

A similar pair of laws hold for 'joinT' and 'meetT'. In a /bi/-lattice we also
have negation 'negT' satisfying the following:

@
-- De Morgan operation for joinT and meetT
'negT' (d \``joinT`\` d') = 'negT' d \``meetT`\` 'negT' d'
'negT' (d \``meetT`\` d') = 'negT' d \``joinT`\` 'negT' d'

-- double negation
'negT' ('negT' d) = d

-- commutes with joinK and meetK
'negT' (d \``joinK`\` d') = 'negT' d \``joinK`\` 'negT' d'
'negT' (d \``meetK`\` d') = 'negT' d \``meetK`\` 'negT' d'
@

The dual operation to 'negT' is 'negK'.
-}
-- TODO consider multiple newtype instances of typeclasses in lattice package?
-- feels more clumsy. maybe instead roll my own bilattice type class
-- | Join (least upper bound) in the Belnap lattice along the @Knowledge@ ordering
joinK :: Dec -> Dec -> Dec
joinK Gap d      = d
joinK d Gap      = d
joinK Conflict _ = Conflict
joinK _ Conflict = Conflict
joinK Grant Deny = Conflict
joinK Deny Grant = Conflict
joinK d _        = d -- Grant, Grant; Deny, Deny

-- | Meet (greatest lower bound) in the Belnap lattice along the @Knowledge@
-- ordering
meetK :: Dec -> Dec -> Dec
meetK Conflict d = d
meetK d Conflict = d
meetK Gap _      = Gap
meetK _ Gap      = Gap
meetK Grant Deny = Gap
meetK Deny Grant = Gap
meetK d _        = d -- Grant, Grant; Deny, Deny

-- | Join (least upper bound) in the Belnap lattice along the @Truth@ ordering
joinT :: Dec -> Dec -> Dec
joinT Deny d       = d
joinT d Deny       = d
joinT Grant _      = Grant
joinT _ Grant      = Grant
joinT Conflict Gap = Grant
joinT Gap Conflict = Grant
joinT d _          = d -- Conflict, Conflict; Gap, Gap

-- | Meet (greatest lower bound) in the Belnap lattice along the @Truth@ ordering
meetT :: Dec -> Dec -> Dec
meetT Grant d      = d
meetT d Grant      = d
meetT Deny _       = Deny
meetT _ Deny       = Deny
meetT Conflict Gap = Deny
meetT Gap Conflict = Deny
meetT d _          = d -- Conflict, Conflict; Gap, Gap

-- | Negation in the Belnap lattice along the @Knowledge@ ordering
negK :: Dec -> Dec
negK Gap      = Conflict
negK Conflict = Gap
negK d        = d

-- | Negation in the Belnap lattice along the @Truth@ ordering
negT :: Dec -> Dec
negT Grant = Deny
negT Deny  = Grant
negT d     = d

-- | A /rule/ specifies the conditions under which an access request is granted
-- or denied
data Rule = GrantIf Cond | DenyIf Cond deriving Show

-- | A guard expresses one or more statements of the form
--
-- @
-- pol '~~>' dec
-- @
-- where @pol :: 'Pol'@ and @dec :: 'Dec'@
--
-- Guards are combined with '.&.' as follows
--
-- @
-- p1 '~~>' d1 '.&.'
-- p2 '~~>' d2 '.&.'
-- p3 '~~>' d3 -- ...etc.
-- @
data Guard = Truth | Eval Pol Dec | Conj Guard Guard deriving Show

-- | The "evaluates to" relation
(~~>) :: Pol -> Dec -> Guard
(~~>) = Eval

{- $policies

Policies come in several forms, though typically they will be /rules/ like
"grant if the following conditions holds...". For example, we can take the
condition from earlier and express it as the "body" of a (zero-obligation)
grant-if policy by applying it to 'grantIf0':

@
'grantIf0' ((subj =:= name "driver")  &:&
          (obj  =:= name "vehicle") &:&
          (act  =:= name "locate"   |:| (act         =:= name "unlock"
                                    &:&  name "time" <:= name "deadline")))
@

This expression of type 'Pol' represents a policy that grants whenever the
following are satisfied:

* the 'subj'ect matches @"driver"@

* the 'obj'ect they request access to is some known @"vehicle"@

* the 'act'ion they request to perform on it is either @"locate"@ or, if it is
  instead @"unlock"@, that @"time"@ is no greater than @"deadline"@.

There is also a dual 'denyIf0' rule. Both are in fact specialised forms of the
more general policy combinators 'grantIf' and 'denyIf' that accept obligations.
Another basic form of policy is also the simplest - the /constant/ policies:

* 'grant'
* 'deny'
* 'gap'
* 'confl'

These correspond to policies that always evaluate to 'Grant', 'Deny', 'Gap' and
'Conflict', respectively.

/Composite/ policies are formed with the 'Case' constructor. Let @p :: 'Pol'@.
An example of a composite policy that "negates" @p@ along the @Knowledge@
ordering is as follows:

@
'Case' [(p '~~>' Conflict, 'gap'),    -- case 1
      (p '~~>' Gap,      'confl')]  -- case 2
                       p        -- default
@

As with all composite policies, this expression consists of a list of
'Guard's, each paired with a 'Pol'icy. Intuitively, 'Case' expressions resemble
case-statements in other programming languages: if evaluation of @p@ conflicts
i.e. the guard @p ~~> Conflict@, then return @gap@. If instead @p@ gaps, then
return @confl@. If none of the guards hold, return the /default/ - the only
policy outside the list - in this instance @p@.

Several /policy idoms/ such as 'denyByDefault' and priority composition '>:>'
have their own combinator functions, but others can be defined in a similar
fashion using 'Case' expressions like the one above.
-}

-- | A type of policies. Construct basic policies (rules, constants) with
-- combinator functions. Composites can be constructed with 'Case'.
data Pol = Konst Dec | Filter Rule [Oblg]
         | Case [(Guard, Pol)] Pol -- ^ Composite policies
           deriving Show

-- | Identifier for the /subject/ of an access request
subj = ATerm (Keyword Subj)
-- | identifier for the target /object/ of an access request
obj  = ATerm (Keyword Obj)
-- | Identifier for the /action/ to perform of an access request
act  = ATerm (Keyword Act)
-- | Constructs a named domain-specific entity
name = ATerm . Entity

grant, deny, gap, confl :: Pol
-- | Constant 'Grant' policy
grant = Konst Grant
-- | Constant 'Deny' policy
deny  = Konst Deny
-- | Constant 'Gap' policy
gap   = Konst Gap
-- | Constant 'Conflict' policy
confl = Konst Conflict

grantIf, denyIf :: Cond -> [Oblg] -> Pol
-- | @grantIf cond obls@ constructs a policy that grants with obligations @obls@
-- if the conditions @cond@ is satisfied and gaps otherwise
grantIf cond = Filter (GrantIf cond)
-- | @denyIf cond obls@ constructs a policy that denies with obligations @obls@
-- if the conditions @cond@ is satisfied and gaps otherwise
denyIf  cond = Filter (DenyIf  cond)

-- 0-obligation rule policy constructors
grantIf0, denyIf0 :: Cond -> Pol
-- | Constructs a zero-obligation policy that grants if the given condition is
-- satisfied and gaps otherwise
grantIf0 cond = Filter (GrantIf cond) []
-- | Constructs a zero-obligation policy that denies if the given condition is
-- satisfied and gaps otherwise
denyIf0  cond = Filter (DenyIf  cond) []

-- | Generalisation of a rule: for a policy @p@ and target condition @t@,
-- constructs a policy that applies @p@ if @t@ is satisfied
--
-- /Note: notion of target similar to that of XACML/
ifTarget :: Pol -> Cond -> Pol
ifTarget pol t = Case [(grantIf0 t ~~> Grant, pol)] gap

-- | One form of priority composition: given policies @p@ and @q@, constructs a
-- policy that denies if @p@ conflicts, defers to @q@ if it gaps and otherwise
-- acts as @p@
infixr 5 >:> -- TODO decide on a convention for precedence
(>:>) :: Pol -> Pol -> Pol
p >:> q = Case [(p ~~> Conflict, deny),
                (p ~~> Gap,      q)]
                                 p
-- TODO right-associative infix

-- | Atomic condition representing truth
true = T

-- | Atomic condition representing falsity
false = Not T

-- TODO fix operator precedence
(=:=), (>:=), (<:=), (<:), (>:) :: Term -> Term -> Cond
-- | 'Term' equality relation
(=:=) = BinRel Equ
-- | 'Term' greater-than-or-equal relation
(>:=) = BinRel Gte
-- | 'Term' less-than-or-equal relation
(<:=) = BinRel Lte
-- | 'Term' less-than relation
(<:)  = BinRel Lt
-- | 'Term' greater-than relation
(>:)  = BinRel Gt

-- | Conjunction of conditions
(&:&) = And
-- | Disjunction of conditions
(|:|) = Or
-- | Negation of conditions
(¬)   = Not

-- | Combines two guards
infixr 6 .&.
(.&.) :: Guard -> Guard -> Guard
(.&.) = Conj

-- | Wraps a policy so that its non-'Grant' decisions become 'Deny'
denyByDefault :: Pol -> Pol
denyByDefault p = Case [(p ~~> Grant, p)] deny

-- | Merges the information content of two policies
--
-- More precisely: the lifting of 'joinK' to the 'Pol' level
join :: Pol -> Pol -> Pol
join p q = Case [(p ~~> Gap,      q    ),
                 (q ~~> Gap,      p    ),
                 (p ~~> Conflict, confl),
                 (q ~~> Conflict, confl),
                 (p ~~> Grant .&.
                  q ~~> Deny,     confl),
                 (q ~~> Grant .&.
                  p ~~> Deny,     confl)]
                                  p

toCase :: (Dec -> Maybe Pol) -> Pol -> Pol
toCase f p = Case [(p ~~> d, fromJust (f d)) | d <- [Grant, Deny, Gap, Conflict]
                                             , isJust (f d)]
                  p

-- NOTE maybe not so useful: in practice continuations depend on p
negateK' = toCase table
  where
    table Gap      = Just confl
    table Conflict = Just gap
    table _        = Nothing

--------------------------------------------------------------------------------

-- | A type of environments
newtype Env = Env { unEnv :: [(ATerm, Val)] }
-- NOTE why not just type? to hide the implementation

-- | Values of 'Term's
data Val = ValI Integer
         | ValB Bool
         | ValS String
         | NoVal
           deriving (Eq, Ord, Show)

evalT :: Term -> Env -> Val
evalT (LitI n) _    = ValI n
evalT (LitB b) _    = ValB b
evalT (LitS s) _    = ValS s
evalT (ATerm t) env = fromMaybe NoVal $ lookup t $ unEnv env

evalG0 :: Guard -> Env -> Bool
evalG0 Truth _            = True
evalG0 (Eval pol dec) env = evalP0 pol env == dec
evalG0 (Conj g1 g2) env   = evalG0 g1 env && evalG0 g2 env

evalC :: Cond -> Env -> Bool
evalC T _                    = True
evalC (Not c)     env        = not (evalC c env)
evalC (And c1 c2) env        = evalC c1 env && evalC c2 env
evalC (Or  c1 c2) env        = evalC c1 env || evalC c2 env
evalC (BinRel Equ t1 t2) env = evalT t1 env == evalT t2 env
evalC (BinRel Lt  t1 t2) env = evalT t1 env <  evalT t2 env
evalC (BinRel Lte t1 t2) env = evalT t1 env <= evalT t2 env
evalC (BinRel Gt  t1 t2) env = evalT t1 env >  evalT t2 env
evalC (BinRel Gte t1 t2) env = evalT t1 env >= evalT t2 env
-- NOTE some comparisons won't even make sense but keep things simple by letting
-- Haskell decide (via deriving Ord). maybe refine later with a type checker

evalG :: Guard -> Env -> Writer [Oblg] Bool
evalG Truth _ = return True
evalG (Eval pol dec) env = do
  d <- clearIf (evalP pol env) (\d -> d /= dec || d `elem` [Gap, Conflict])
  return (d == dec)
evalG (Conj g1 g2) env = (&&) <$> evalG g1 env <*> evalG g2 env

-- | Evaluator for 'Pol'icies
evalP :: Pol -> Env -> Writer [Oblg] Dec
evalP (Konst dec) _ = return dec
evalP (Filter (GrantIf cond) obls) env
  | evalC cond env = writer (Grant, obls)
  | otherwise      = return Gap
evalP (Filter (DenyIf cond) obls) env
  | evalC cond env = writer (Deny, obls)
  | otherwise      = return Gap
evalP (Case [] defPol) env = evalP defPol env
evalP (Case ((g, p) : as) defPol) env = do
  b <- clearIf (evalG g env) not
  if b then evalP p env
       else evalP (Case as defPol) env

-- https://stackoverflow.com/questions/34832072/what-is-the-point-of-pass-and-listen-in-writer-monad
clearIf :: MonadWriter w m => m a -> (a -> Bool) -> m a
clearIf xm pred = pass $ do
  x <- xm
  return (x, if pred x then const mempty else id)

-- | Evaluator for zero-obligation 'Pol'icies
evalP0 :: Pol -> Env -> Dec
evalP0 (Konst dec) _                   = dec
evalP0 (Filter (GrantIf c) _) env      = if evalC c env then Grant else Gap
evalP0 (Filter (DenyIf c) _) env       = if evalC c env then Deny  else Gap
evalP0 (Case [] defPol) env            = evalP0 defPol env
evalP0 (Case ((g, p) : as) defPol) env =
  evalP0 (if evalG0 g env then p else Case as defPol) env

oblg :: Dec -> Pol -> Env -> [Oblg]
oblg _ (Konst _) _ = []
oblg Grant (Filter (GrantIf cond) obls) env
  | evalC cond env = obls
  | otherwise = []
oblg Deny (Filter (DenyIf cond) obls) env
  | evalC cond env = obls
  | otherwise = []
oblg _ (Filter _ _) _ = []
oblg dec (Case arms defPol) env = oblg' dec gd env ++ oblg dec pol env
  where
    arms' = dropWhile (not . flip evalG0 env . fst) arms
    (gd, pol) = if null arms' then (Truth, defPol) else head arms'

oblg' :: Dec -> Guard -> Env -> [Oblg]
oblg' _ Truth _ = []
oblg' Grant (Eval pol Grant) env = oblg Grant pol env
oblg' Deny  (Eval pol Deny)  env = oblg Deny  pol env
oblg' _ (Eval _ _) _ = []
oblg' dec (Conj g1 g2) env = oblg' dec g1 env ++ oblg' dec g2 env

-- TODO remove these
-- gives the next policy the power to overturn (i.e. deny) your grant
overriddenBy, overriddenBy' :: Pol -> Pol -> Pol
overriddenBy p q = Case [(Eval p Grant `Conj` Eval q Deny, Konst Deny)] p
overriddenBy' p q = Case [(Eval p Grant
                    `Conj` Eval q Deny, Filter (DenyIf T) [Log 0])] p

polG, polD :: Pol
polG = Filter (GrantIf T) [Log 1]
polD = Filter (DenyIf T)  [Log 2]

--------------------------------------------------------------------------------

-- | Transforms a policy into a lower-level 'Circuit' representation
compile0 :: Pol -> Circuit
compile0 p = Circuit (goc p) (doc p)

-- | Returns the conditions that hold just when the given policy grants or
-- conflicts
goc :: Pol -> Cond
goc (Konst Grant) = T
goc (Konst Conflict) = T
goc (Konst _) = false
goc (Filter (GrantIf cond) _) = cond
goc (Filter (DenyIf _) _) = false
goc (Case [] defPol) = goc defPol
goc (Case arms defPol) = compCase True arms defPol

-- | Returns the conditions that hold just when the given policy denies or
-- conflicts
doc :: Pol -> Cond
doc (Konst Deny) = T
doc (Konst Conflict) = T
doc (Konst _) = false
doc (Filter (GrantIf _) _) = false
doc (Filter (DenyIf cond) _) = cond
doc (Case [] defPol) = doc defPol
doc (Case arms defPol) = compCase False arms defPol

-- TODO rename t
compPos :: Guard -> Cond
compPos Truth = T
compPos (Eval pol Conflict) = goc pol `And` doc pol
compPos (Eval pol Gap) = Not (goc pol) `And` Not (doc pol)
compPos (Eval pol Grant) = goc pol `And` Not (doc pol)
compPos (Eval pol Deny) = Not (goc pol) `And` doc pol
compPos (Conj g1 g2) = compPos g1 `And` compPos g2

-- NOTE deprecated
compNeg :: Guard -> Cond
compNeg (Eval pol Conflict) = Not (goc pol) `Or` Not (doc pol)
compNeg (Eval pol Gap) = goc pol `Or` doc pol
compNeg (Eval pol Grant) = Not (goc pol) `Or` doc pol
compNeg (Eval pol Deny) = goc pol `Or` Not (doc pol)
compNeg (Conj g1 g2) = compNeg g1 `Or` compNeg g2

-- assume nonempty; none of the guards hold except the last
disjunct :: Bool -> [(Guard, Pol)] -> Cond
disjunct b arms = foldr (And . Not . compPos . fst) (compPos trueGuard) pairs
                  `And` compPol pol
  where
    (pairs, [(trueGuard, pol)]) = splitAt (length arms - 1) arms
    compPol = if b then goc else doc

lastDisjunct :: Bool -> [Guard] -> Pol -> Cond
lastDisjunct b gs pol = foldr (And . Not . compPos) T gs
                        `And` compPol pol
  where
    compPol = if b then goc else doc

compCase :: Bool -> [(Guard, Pol)] -> Pol -> Cond
compCase isGrant arms defPol =
  foldr (Or . disjunct isGrant) (lastDisjunct isGrant guards defPol) armInits
  where
    armInits = tail (inits arms)
    guards = map fst arms

-- | Lifting of 'negT' to the 'Pol' level
neg :: Pol -> Pol
neg p = Case [(p ~~> Grant, deny),
              (p ~~> Deny,  grant)]
                            p

evalOneOf :: Pol -> [Dec] -> Pol -> [(Guard, Pol)]
evalOneOf p1 ds p2  = map (\d -> (Eval p1 d, p2)) ds

--------------------------------------------------------------------------------

{- $compilation

Any policy @p :: 'Pol'@ can be expressed in a canonical /join normal form/
that is completely determined by a pair of "circuits", expressed in the
same language as conditions:

@
-- join normal form law
p = 'grantIf0' ('goc' p) \``join`\` 'denyIf0' ('doc' p)
@

The rationale for the circuits @'goc' p@ and @'doc' p@ as a lower-level
representation is two-fold: they can be evaluated in a standard, predictable way
- potentially on devices with limited capacity - and they also form a basis for
static analyses of policies via other tooling.

Use 'compile0' to transform a policy into a 'Circuit' pair:

>>> compile0 p

Internally, it uses the compilation functions 'goc' and 'doc' to generate the
required conditions. Furthermore, circuits are serialisable into JSON form, by
using one of the family of 'encode' functions. For example:

>>> encodeFilePretty "my-policy.json" (compile0 p)

The function 'encodeFilePretty' encodes the circuits from @'compile0' p@ into a
pretty-printed byte string that is written to the supplied file path.
-}

-- mini-dictionary of some JSON keys / values
[op, typ, boolTyp, intTyp, strTyp, val, attrs] =
  ["operation", "type", "boolean", "int64", "string", "value", "attribute_list"]

-- | A lower-level representation of policies comprising a pair of /circuits/
-- i.e. the 'goc' and 'doc' conditions
data Circuit = Circuit {
  policy_goc :: Cond,
  policy_doc :: Cond } deriving (Show, Generic)

-- TODO stub for oblg circuits: how to represent "empty"?

-- to make Circuit an instance of ToJSON, you need an implementation of toJSON
-- as a minimum. but note that the "deriving Generic" writes toJSON for you. the
-- aeson docs recommend you almost always implement toEncoding, for "direct
-- encoding". otherwise it'll end up using toJSON i.e. construct an intermediate
-- Value first, so less effificent. minimum implementation:

instance ToJSON Circuit where
  toEncoding = genericToEncoding defaultOptions

-- for Cond, i need it in a particular format, so the generic one won't do. i
-- have to implement toJSON by hand, building the object. oddly, in this
-- situation the toEncoding looks almost the same, except you use pairs instead
-- of object, on a monoid of key-value pairs (combined with <>) rather than a
-- list. factored this into a pattern as below

instance ToJSON Cond where
  toJSON     = object . cond2kvps
  toEncoding = pairs . mconcat . cond2kvps

cond2kvps :: KeyValue a => Cond -> [a]
cond2kvps T                   = [typ .= boolTyp, val .= True]
cond2kvps (Not T)             = [typ .= boolTyp, val .= False]
cond2kvps (Not c)             = [op .= String "not", attrs .= [c]]
cond2kvps (And c1 c2)         = [op .= String "and", attrs .= [c1, c2]]
cond2kvps (Or c1 c2)          = [op .= String "or",  attrs .= [c1, c2]]
cond2kvps (BinRel pred t1 t2) = [op .= pred,         attrs .= [t1, t2]]

-- the generically derived toJSON for BinPred (alone) isn't quite what i want:
-- constructor names start with an uppercase letter but i want this to be
-- lowercase. so i override the defaultOptions with the appropriate
-- customisation, and pass that to genericToJSON. toEncoding similar

-- NOTE eq now called equ...
instance ToJSON BinPred where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

customOptions = defaultOptions {
    constructorTagModifier = \(c:cs) -> C.toLower c : cs }

-- similar to Cond, Term needs to be rendered in a particular way, so i don't use
-- the generically derived toJSON for that

instance ToJSON Term where
  toJSON     = object . term2kvps
  toEncoding = pairs . mconcat . term2kvps

-- NOTE for now, say terms are "untyped"
term2kvps :: KeyValue a => Term -> [a]
term2kvps (LitI n) = [typ .= intTyp,           val .= n]
term2kvps (LitB b) = [typ .= boolTyp,          val .= b]
term2kvps (LitS s) = [typ .= strTyp,           val .= T.pack s]
term2kvps t        = [typ .= String "untyped", val .= T.pack (show t)]
-- NOTE pack needed otherwise prob uses [Char] instance of ToJSON

--------------------------------------------------------------------------------

-- | 'encodePretty' but with the byte string decoded to UTF-8 and printed
--
-- /Useful for testing in GHCi for example/
penc :: ToJSON a => a -> IO ()
penc = TIO.putStrLn . TE.decodeUtf8 . encodePretty

-- | Similar to 'encodeFile' but uses 'encodePretty' instead of plain 'encode'
encodeFilePretty :: ToJSON a => FilePath -> a -> IO ()
encodeFilePretty fp = B.writeFile fp . encodePretty

