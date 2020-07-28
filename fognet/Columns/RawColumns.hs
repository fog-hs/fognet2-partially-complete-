{-# Language
TypeFamilies
#-}

module Columns.RawColumns where

import TypeLevel.Nat
import Containers.Sized


newtype RawColumns a = RawColumns [[a]] deriving Show

instance HasSize (RawColumns a) where
 type SizeType (RawColumns a) = [Nat]

instance Sized (RawColumns a) where
 getSize (RawColumns a) = map (toNat . length) a



