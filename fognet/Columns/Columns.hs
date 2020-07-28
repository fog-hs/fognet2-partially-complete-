{-# Language
 RankNTypes
,TypeOperators
,UndecidableInstances
,TypeApplications
,KindSignatures
,ScopedTypeVariables
,DataKinds
,GADTs
,FlexibleInstances
,FlexibleContexts
,TypeFamilies
,MultiParamTypeClasses
#-}

-- NB.
-- cant infer size from unsized,
-- but can throuw runtime error if user specified size is wrong.

module Columns.Columns (module Columns.Columns,module Columns.RawColumns)where

import Data.Proxy

import TypeLevel.Nat
import Containers.List
import Containers.Sized
import Columns.RawColumns

data Columns (lengths :: [Nat]) a = Columns [[a]] deriving Show


emptyColumns :: Columns '[] a
emptyColumns = Columns [] -- toColumns (Proxy @( '[])) (RawColumns [[]])

consColumn :: (ToUnsized (List n a),IsNat n,GetNats lengths) => List n a -> Columns lengths a -> Columns (n ': lengths) a
consColumn l (Columns xs) = Columns $ (toUnsized l) : xs

unconsColumns :: (IsList (List n a),IsNat n,GetNats lengths) => Columns (n ': lengths) a -> (List n a,Columns lengths a)
unconsColumns (Columns (x:xs)) = ((toList Proxy x),Columns xs)
unconsColumns _ = error "unconsColumns on empty Columns"


toColumns :: forall lengths a. (ToSized (RawColumns a) lengths,GetNats lengths) => Proxy (lengths :: [Nat]) -> RawColumns a -> Columns lengths a
toColumns _ = toSized (getNats (Proxy @lengths))

toColumns' :: forall lengths a. (ToSized (RawColumns a) lengths,GetNats lengths) => Proxy (lengths :: [Nat]) -> [[a]] -> Columns lengths a
toColumns' p = toColumns p . RawColumns


----
-- Size API for Columns

instance HasSize (Columns lengths a) where
 type SizeType (Columns lengths a) = [Nat]

-- does not follow the convention of IVec etc
-- where the Raw version is wrapped by a newtype
-- but that is because there are no accessors provided
-- so no need to be able to utilise these directed by newtype instances

instance GetNats lengths => Sized (Columns lengths a) where
 getSize _ = getNats (Proxy @lengths)

instance GetNats lengths => HasSized (RawColumns a) (lengths :: [Nat]) where
 type SizedVersion (RawColumns a) lengths = Columns lengths a

instance ToSized (RawColumns a) '[] where
 toSized [] (RawColumns []) = emptyColumns 
 toSized a _  = {- ALERT DANGER !! -} error $ " column length mismatch, try supplying the correct size; " ++ show a 
 
-- this is terrible
-- it needs the first argument to be singleton so it can be matched
testCol :: Columns '[Zero] Int
--test = toSized [[(),()],[()]] $  RawColumns [[]::[Int]]
testCol = toSized [[]] $  RawColumns [[]::[Int]]
--test = toSized [] $  RawColumns [[]::[Int]] -- these shouldnt even compile...
 

instance (ToSized (RawColumns a) lengths,GetNats lengths,IsList (List n a)) => ToSized (RawColumns a) (n ': lengths) where
 toSized (_:ls) (RawColumns (x : xs)) = consColumn (toList (Proxy @n) x) (toSized ls (RawColumns xs))
 toSized [] _ = {- ALERT DANGER !! -} error $ " column length mismatch, try supplying the correct size; was supplied [])"

instance GetNats lengths => HasUnsized (Columns lengths a) where
 type UnsizedVersion (Columns lengths a) = RawColumns a

instance GetNats lengths => ToUnsized (Columns lengths a) where
 toUnsized (Columns a) = RawColumns a

type family IsColumns x where
 IsColumns (Columns lengths a) = (AreNats lengths,ToUnsized (Columns lengths a),ToSized (RawColumns a) lengths)
