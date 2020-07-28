{-# Language 
 PolyKinds 
,ConstraintKinds
,UndecidableInstances
,FlexibleInstances
,UndecidableSuperClasses
,TypeApplications
,AllowAmbiguousTypes
,ScopedTypeVariables
,TypeOperators
,TypeSynonymInstances
,MultiParamTypeClasses
,RankNTypes
,GADTs
,FlexibleContexts
,TypeFamilies
,DataKinds 
-- ,StandaloneKindSignatures
#-}

import Data.Proxy
import GHC.Exts (Constraint)

import Containers.Container
import Containers.Sized
import Containers.List
import TypeLevel.BoundedInt
import Columns.Columns
import Vectors.IVec
import Columns.IColumns

import BIntMatrix
import Snocs
import Scaffold

class (Functor (NetContainer netWrapper), Container (NetContainer netWrapper)) => NeuralNet (netWrapper :: [Nat] -> * -> *) where
 type NetContainer netWrapper :: * -> *
 unwrapNet :: forall container lengths a. container ~ NetContainer netWrapper => (netWrapper lengths a) -> container (container a) -- isnt actually supposed to be used, but must be able to provide it
 netMap :: (a -> b) -> NetContainer netWrapper (NetContainer netWrapper a)
                    -> NetContainer netWrapper (NetContainer netWrapper b)
 netMap f xs = fmap (fmap f) xs
 containerFromList :: [a] -> NetContainer netWrapper a
 
protoNetWrapper :: forall netWrapper lengths a container. (container ~ NetContainer netWrapper,GetNats lengths,NeuralNet netWrapper) => Proxy netWrapper -> [[[a]]] -> container (container (container a)) 
protoNetWrapper _ xs = containerFromList @netWrapper (map (containerFromList @netWrapper . map (containerFromList @netWrapper))xs)

instance NeuralNet IColumns where
 type NetContainer IColumns = RawIVec 
 unwrapNet (IColumns a) = a
 containerFromList = toRawIVec

data Net (netWrapper :: [Nat] -> * -> *) (lengths :: [Nat]) where
 Net :: (NeuralNet netWrapper,container ~ NetContainer netWrapper) => container (container (container (Int,Double))) -> Net netWrapper lengths 

instance (NeuralNet netWrapper,container ~ NetContainer netWrapper,
 Show (container (container (container (Int, Double))))) => Show (Net netWrapper lengths) where
 show (Net (x :: container (container (container (Int, Double))))) = show @(container (container (container (Int, Double)))) x

toNet :: forall netWrapper lengths. (GetNats lengths,NeuralNet netWrapper) => netWrapper lengths (NetContainer netWrapper Int) -> Net netWrapper lengths 
toNet = Net . netMap @netWrapper  (mapC (\(i::Int) -> (i,0::Double))) .  unwrapNet 

toNet' :: forall netWrapper container lengths. (container ~ NetContainer netWrapper,GetNats lengths,NeuralNet netWrapper) => container (container (container Int)) -> Net netWrapper lengths 
toNet' = Net . netMap @netWrapper  (mapC (\(i::Int) -> (i,0::Double))) 

initialiseNet :: forall netWrapper lengths. (FromScaffold (ToSnocs lengths),NeuralNet netWrapper,ScaffoldCreationConstraint lengths) => Scaffold (ToSnocs lengths) -> Net netWrapper lengths 
initialiseNet scaffold = toNet' $ (protoNetWrapper @netWrapper @lengths Proxy)  $ fromScaffold scaffold
 where
  f :: [[[Int]]] -> netWrapper lengths (NetContainer netWrapper Int)
  f = undefined

createNet :: (FromScaffold (ToSnocs lengths),GetNats lengths,NeuralNet netWrapper,ScaffoldCreationConstraint lengths) 
 => Proxy (lengths :: [Nat]) -> Net netWrapper lengths
createNet p = initialiseNet $ fullyConnectedScaffold p 


-- foldable constraint confused by existence of foldrC - should that approach be depreciated?
runNet :: forall netWrapper lengths. (Index (NetContainer netWrapper) ~ Int,Foldable (NetContainer netWrapper)) => Net netWrapper lengths -> NetContainer netWrapper Double -> NetContainer netWrapper Double
runNet (Net xs) a = foldrC f a xs
 where
  f :: NetContainer netWrapper (NetContainer netWrapper (Int, Double))
           -> NetContainer netWrapper Double 
           -> NetContainer netWrapper Double
  f a b =  (mapC (tanh . sumC . g b) a)
--(zipWithC (*) (g a (mapC snd a)) (repeat b)))
  g :: NetContainer netWrapper Double
           -> NetContainer netWrapper (Int, Double)
           -> NetContainer netWrapper Double
  g a b = mapC (h a) b
  h :: NetContainer netWrapper Double -> (Int, Double) -> Double
  h a (i,x) = (access a i) * x


------------------------
-- testing

type family Replicate (n :: Nat) (a::k) :: [k] where
 Replicate Zero a = '[]
 Replicate (Succ n) a = a ': Replicate n a 

egNet :: (FromScaffold (ToSnocs lengths),GetNats lengths,NeuralNet netWrapper,ScaffoldCreationConstraint lengths
 ,lengths ~ Replicate (ToNat 1) (ToNat 1)) => Proxy netWrapper -> Net netWrapper lengths
egNet _ = createNet Proxy 

updateNet :: forall netWrapper lengths. Index (NetContainer netWrapper) ~ Int => ((Int,Int,Int) -> Double -> Double) -> Net netWrapper lengths -> Net netWrapper lengths
updateNet f (Net xs) = Net $ imapC (g f) xs
 where
  g f i xs = imapC (h f i) xs
  h f i j xs = imapC (e f i j) xs
  e f i j k (b,d) = (b,f (i,j,k) d)

test = flip runNet (containerFromList @IColumns (replicate 32 1)) (updateNet (const (const (0.1 :: Double))) (egNet (Proxy @IColumns )))

test2 = flip runNet (containerFromList @IColumns (replicate 32 1)) (updateNet (const (const (0.1 :: Double))) 
             (createNet @_ @IColumns (Proxy :: Proxy '[ToNat 2]) ))

test3 = flip runNet (containerFromList @IColumns (replicate 32 1)) (updateNet (const (const (0.1 :: Double))) 
   (createNet @_ @IColumns (Proxy :: Proxy '[ToNat 2,ToNat 2]) ))

test4 n = flip runNet (containerFromList @IColumns (replicate 256 1)) (updateNet (const (const (n :: Double))) 
   (createNet @_ @IColumns (Proxy :: Proxy (Replicate (ToNat 64) (ToNat 8))) ))

go2 = sum $ concat $  traverse (test4) [0,0.1..0.3]

