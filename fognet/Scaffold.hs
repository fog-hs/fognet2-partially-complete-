{-# Language 
 PolyKinds 
,RankNTypes
,TypeApplications
,AllowAmbiguousTypes
,ScopedTypeVariables
,TypeOperators
,DataKinds 
,GADTs
,ConstraintKinds
,FlexibleContexts
,FlexibleInstances
,UndecidableInstances
,MultiParamTypeClasses
#-}

module Scaffold where

import Containers.List
import TypeLevel.BoundedInt
import Columns.Columns
import Containers.Container
import Containers.Sized
import Data.Proxy
import GHC.Exts (Constraint)

import BIntMatrix
import Snocs

type FirstLayer n = BIntMatrix n n 

firstScaffoldLayer :: BIntMatrixConstraint n => Proxy (n :: Nat) -> FirstLayer n
firstScaffoldLayer p = toList p (take (fromNat (getNat p)) (map (return.toBoundedInt) [0..]))

data Scaffold (ns :: Snocs Nat) where
 InitScaffold :: FirstLayer n -> Scaffold (SnocOne n)
 ScaffoldSnoc :: Scaffold (Snoc ns n1) -> BIntMatrix n2 n1 -> Scaffold (Snoc (Snoc ns n1) n2)

instance BIntMatrixConstraint n => Show (Scaffold (SnocOne n)) where
 show (InitScaffold x) = l x
  where
   l :: FirstLayer n -> String
   l = show

instance (IsNat n,IsNat n2,Show (Scaffold ('Snoc ns n2)),Functor (List n),ToUnsized (List n [BoundedInt n2]),ToSized [[BoundedInt n2]] n) => Show (Scaffold (Snoc (Snoc ns n2) n)) where
 show (ScaffoldSnoc xs x) = show xs ++ " | " ++ show x -- "show scaffold snocs" -- (InitScaffold x) = show x

unScaffoldSnoc :: forall ns n1 n2. Scaffold (Snoc (Snoc ns n1) n2) -> (Scaffold (Snoc ns n1),BIntMatrix n2 n1)
unScaffoldSnoc (ScaffoldSnoc xs x) = (xs,x) 

type CreateScaffoldConstraints n ns lengths =  () :: Constraint

class CreateScaffold snocs lengths where
 createScaffold' :: Scaffold snocs
                 -> Columns lengths [Int]
                 -> Scaffold (MashSnocs snocs lengths)

instance 
 (IsList (List n [Int])
 ,GetNats xs
 ,CreateScaffold ('Snoc ('Snoc sn s) n) xs
 ,IsNat s
 ,Functor (List n)
 ) 
 =>CreateScaffold ('Snoc sn s) (n ': xs) where
 createScaffold' scaffoldSoFar cs = createScaffold' x cs'  -- performs bounds check
  where
   x :: Scaffold ('Snoc ('Snoc sn s) n)
   x = (ScaffoldSnoc y z)
   z :: BIntMatrix n s
   z = (fmap (map toBoundedInt) c)
   y :: Scaffold ('Snoc sn s)
   y = scaffoldSoFar 
   (c,cs') = unconsColumns cs

instance Show (Scaffold snocs) => CreateScaffold snocs '[] where
 createScaffold' scaffoldSoFar (Columns []) = scaffoldSoFar 
 createScaffold' a _ = error $  "createScaffold' error! " ++ show a

type ProtoScaffold (lengths :: [Nat])= Columns lengths [Int]

createScaffold 
 :: (CreateScaffold (SnocOne n) lengths
    ,IsList (List n [BoundedInt n])
    ,IsNat n
    )
 => Proxy n
 -> ProtoScaffold lengths
 -> Scaffold (MashSnocs (SnocOne n) lengths)
createScaffold p cs = createScaffold' ( InitScaffold (firstScaffoldLayer p)) cs


fromScaffold :: forall lengths. FromScaffold lengths => Scaffold (lengths :: Snocs Nat) -> [[[Int]]]
fromScaffold xs = reverse (fromScaffold' @lengths xs)

class FromScaffold (lengths :: Snocs Nat) where
  fromScaffold' :: Scaffold lengths -> [[[Int]]]

instance (IsList (List n [BoundedInt n]),IsNat n) => FromScaffold (SnocOne n) where
 fromScaffold' (InitScaffold x) = [fromBIntMatrix x]

instance 
 (IsList (List n2 [BoundedInt n1])
 -- ,IsList (List n [BoundedInt n])
 ,FromScaffold ('Snoc ns n1)) 
 => FromScaffold (Snoc (Snoc ns n1) n2) where
 fromScaffold' (s ) = (fromBIntMatrix x) : (fromScaffold' xs)
  where
   (xs,x) = unScaffoldSnoc @ns @n1 @n2 s

performScaffoldBoundsCheck
  :: -- forall {n :: Nat} {lengths :: [Nat]}.
     (FromScaffold (MashSnocs (SnocOne n) lengths),
      CreateScaffold (SnocOne n) lengths, 
      IsList (List n [BoundedInt n])) =>
     Proxy n -> Columns lengths [Int] -> [[[Int]]]
performScaffoldBoundsCheck p cs = fromScaffold $ createScaffold p cs 

protoScaffold :: forall lengths. (GetNats lengths,ToSized (RawColumns [Int]) lengths) => Proxy (lengths :: [Nat]) -> ((Int, Int) -> [[Int]]) -> (ProtoScaffold lengths)
protoScaffold p f = let lengths = map fromNat $ getNats (Proxy @lengths) in toColumns' p $ map f $ zip lengths (init (head lengths : lengths)) 

perpairScaffold :: forall lengths. (CreateScaffold
                          (SnocOne (Head lengths)) (Tail lengths),Functor (List (Head lengths)),IsNat (Head lengths),IsList
                          (List (Head lengths) [BoundedInt (Head lengths)]),ToSized (RawColumns [Int]) (Tail lengths),GetNats (Tail lengths),GetNats lengths,MashSnocs (SnocOne (Head lengths)) (Tail lengths) ~ ToSnocs lengths) => Proxy (lengths :: [Nat]) -> ((Int, Int) -> [[Int]]) -> Scaffold (ToSnocs lengths)
perpairScaffold p f = createScaffold (Proxy @(Head lengths)) (protoScaffold (Proxy @(Tail lengths)) f)

fullyConnectedScaffold :: forall lengths. ScaffoldCreationConstraint lengths => Proxy (lengths :: [Nat]) -> Scaffold (ToSnocs lengths)
fullyConnectedScaffold p = perpairScaffold p f
 where
  f :: (Int, Int) -> [[Int]]
  f (len,prevLen) = replicate len ([0..prevLen -1])

type ScaffoldCreationConstraint lengths = (CreateScaffold(SnocOne (Head lengths)) (Tail lengths),Functor (List (Head lengths)),IsNat (Head lengths),IsList
                          (List (Head lengths) [BoundedInt (Head lengths)]),ToSized (RawColumns [Int]) (Tail lengths),GetNats (Tail lengths),GetNats lengths,MashSnocs (SnocOne (Head lengths)) (Tail lengths) ~ ToSnocs lengths) 
