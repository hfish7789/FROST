module Deleg where
import Lang
import Control.Monad.Writer.Lazy

infixr 5 >+>  -- ballpark: + has precedence 6
(>+>) :: Pol -> Pol -> Pol
p >+> q = Case [(   p ~~> Gap,   denyByDefault q ),
                (   p ~~> Grant
                .&. q ~~> Gap,   grant           ),
                (   p ~~> Grant, denyByDefault q )]
                                 deny

-- if this cannot decide, then it's not just "let next decide". unless next
-- actually grants, everything else will be denied. so where's the delegation?
-- well if this grants, that's not necessarily it - next can't contradict this.
-- the 1st line can only seem to apply for p0 since next is only ever a definite
-- decision...

grantOnGap :: Pol -> Pol
grantOnGap q = Case [(q ~~> Gap, grant)]
                                 (denyByDefault q)

infixr 5 >->
(>->) :: Pol -> Pol -> Pol
p >-> q = Case [(p ~~> Conflict, denyByDefault q ),  -- conflict treated as for gap
                (p ~~> Gap,      denyByDefault q ),  -- as above
                (p ~~> Grant,    grantByDefault q)] -- both gap & conflict can follow grant!
              {- p ~~> Deny -}   deny

grantByDefault :: Pol -> Pol
grantByDefault q = Case [(q ~~> Deny, deny)]
                                      grant

denyByDefault' :: Pol -> Pol
denyByDefault' p = Case [(p ~~> Grant, grant)]
                                       deny

prio :: Pol -> Pol -> Pol
p `prio` q = Case [(p ~~> Grant, q)]
                                 deny

p `op_v` q = Case [(p ~~> Gap,   Case [(q ~~> Grant, grant)]
                                                     deny   ),
                   (p ~~> Grant, Case [(q ~~> Grant, grant),
                                       (q ~~> Gap,   grant)]
                                                     deny   )]
                                 deny
