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

module Demo where
import Lang

cond :: Cond
cond =  subj =:= name "courier"  &:&
       (obj  =:= name "vehicle") &:&
       (act  =:= name "getLocation" |:| (act =:= name "openTrunk"  &:&
                                          (t >:= name "startTime") &:&
                                          (t <:= name "endTime")))
  where
    t = name "currentTime"

pol :: Pol
pol = grantIf0 cond

-- common pattern: deny by default
pol1 :: Pol
pol1 = Case [(pol ~~> Grant, pol)]
                             deny

-- a syntactically valid circuit pair
circ = Circuit cond true
