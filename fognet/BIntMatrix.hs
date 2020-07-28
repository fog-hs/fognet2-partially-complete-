{-# Language 
 KindSignatures
,PolyKinds
,DataKinds
,ConstraintKinds
#-}

module BIntMatrix where

import Containers.List
import TypeLevel.BoundedInt

type RawPList (n :: k) (a :: k -> *) = [a n]

type PList (m :: Nat) (n :: k) (a :: k -> *) = List m (a n)
-- need to use this for fistlayer
-- to ensure that it is bounded contents by its own length

type RawBIntList n = RawPList n BoundedInt

type BIntList m n = PList m n BoundedInt

-- have to rewrite RawPList to avoid defunctionalisation of;
--type RawBIntMatrix n = RawPList n RawBIntList 
type RawBIntMatrix n = [RawBIntList n]
-- can still take length of this (is prev layer length)

type BIntMatrix m n = List m (RawBIntList n)
-- = List m (RawPList n BoundedInt)
-- = List m [BoundedInt n]

-- all this should lead to a Sized instance
fromBIntMatrix :: (IsList (List n (RawBIntList m))) => BIntMatrix n m -> [[Int]]
fromBIntMatrix x = map (map fromBoundedInt) (fromList x)

type BIntMatrixConstraint n = (IsList (BIntMatrix n n),IsNat n)
