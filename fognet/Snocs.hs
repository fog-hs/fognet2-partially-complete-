{-# Language 
TypeOperators
,UndecidableInstances
,DataKinds
,PolyKinds
,KindSignatures
,GADTs
,TypeFamilies
#-}

module Snocs where

data Snocs a where
 EmptySnoc :: Snocs a
 Snoc :: Snocs a -> a -> Snocs a

type SnocOne a = Snoc EmptySnoc a
type ToSnocs (xs :: [a]) = ToSnocs' xs EmptySnoc 

type family ToSnocs' (xs :: [a]) (ys :: Snocs a) :: Snocs a where
 ToSnocs' '[] ys = ys
 ToSnocs' (x ': xs) ys = Snoc (ToSnocs' xs ys) x

type FromSnocs (xs :: Snocs a) = FromSnocs' xs '[] 

type family FromSnocs' (xs :: Snocs a) (ys :: [a]) :: [a] where
 FromSnocs' EmptySnoc ys = ys
 FromSnocs' (Snoc xs x) ys = x ': (FromSnocs' xs ys)

type family MashSnocs (snocs :: Snocs a) (xs :: [a]) :: Snocs a where
 MashSnocs snocs '[] = snocs
 MashSnocs snocs (x ': xs) = MashSnocs (Snoc snocs x) xs

