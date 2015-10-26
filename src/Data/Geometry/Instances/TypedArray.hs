{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MagicHash, UnboxedTuples, PolyKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.TypedArray
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.TypedArray () where

import Data.Foldable (foldr')
import Data.Coerce (coerce)
import GHC.TypeLits (KnownNat)
--import GHCJS.Marshal
--import GHCJS.Types
import Unsafe.Coerce (unsafeCoerce)

import JavaScript.TypedArray
import qualified JavaScript.TypedArray.IO as IO
import qualified Control.Monad.ST as ST
import qualified JavaScript.TypedArray.ST as ST


import Data.Geometry.Prim.JSNum
import Data.Geometry.VectorMath



instance ( ImmutableArrayBufferPrim (TypedArray t)
         ) => ImmutableArrayBufferPrim (TypedArray (Vector n t)) where
    {-# INLINE fromByteArrayPrim #-}
    fromByteArrayPrim ba = coerce (fromByteArrayPrim ba :: TypedArray t)
    {-# INLINE toByteArrayPrim #-}
    toByteArrayPrim arr = toByteArrayPrim (coerce arr :: TypedArray t)

instance ( MutableArrayBufferPrim (SomeTypedArray m t)
         ) => MutableArrayBufferPrim (SomeTypedArray m (Vector n t)) where
    {-# INLINE fromMutableByteArrayPrim #-}
    fromMutableByteArrayPrim mba s = case fromMutableByteArrayPrim mba s of
        (# s1, arr0 :: SomeTypedArray m t #) -> (# s1, coerce arr0 #)
    {-# INLINE toMutableByteArrayPrim #-}
    toMutableByteArrayPrim arr = toMutableByteArrayPrim (coerce arr :: SomeTypedArray m t)

instance ( KnownNat n
         ) => ArrayBufferData (SomeTypedArray m (Vector n t)) where
    {-# INLINE byteLength #-}
    byteLength arr = byteLength (coerce arr :: SomeTypedArray m t)
    {-# INLINE sliceImmutable #-}
    sliceImmutable i0 Nothing arr = coerce $ sliceImmutable
        (i0* dim (undefined :: Vector n t))
        Nothing (coerce arr :: SomeTypedArray m t)
    sliceImmutable i0 (Just i1) arr = coerce $ sliceImmutable
        (i0*m) (Just $ i1*m) (coerce arr :: SomeTypedArray m t)
        where m = i0* dim (undefined :: Vector n t)

instance ( TypedArrayOperations t
         , KnownNat n
         ) => TypedArrayOperations (Vector n t) where
    {-# INLINE typedArray #-}
    typedArray n = coerce (typedArray $ n * dim (undefined :: Vector n t) :: TypedArray t)
    {-# INLINE fillNewTypedArray #-}
    fillNewTypedArray n v = js_fillJSArray (coerce v) n (typedArray n :: TypedArray (Vector n t))
    {-# INLINE fromList #-}
    fromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs) ar0
        where ar0 = typedArray (length vs) :: TypedArray (Vector n t)
    {-# INLINE fromArray #-}
    fromArray arr = coerce (fromArray arr :: TypedArray t)
    {-# INLINE arrayView #-}
    arrayView buf = f $ arrayView buf
        where f :: SomeTypedArray m t -> SomeTypedArray m (Vector n t)
              f = coerce
    {-# INLINE (!) #-}
    arr ! i = coerce $ js_indexVecArray arr i (dim (undefined :: Vector n t))
    {-# INLINE elemSize #-}
    elemSize _ = elemSize (undefined :: TypedArray t) * dim (undefined :: Vector n t)
    {-# INLINE indexOf #-}
    indexOf = undefined
    {-# INLINE lastIndexOf #-}
    lastIndexOf = undefined

instance ( IO.IOTypedArrayOperations t
         , KnownNat n
         ) => IO.IOTypedArrayOperations (Vector n t) where
    {-# INLINE newIOTypedArray #-}
    newIOTypedArray n = coerce
        <$> (IO.newIOTypedArray $ n * dim (undefined :: Vector n t) :: IO (IOTypedArray t))
    {-# INLINE fillNewIOTypedArray #-}
    fillNewIOTypedArray n v = js_fillJSArray (coerce v) n
        <$> (IO.newIOTypedArray n :: IO (IOTypedArray (Vector n t)))
    {-# INLINE newFromList #-}
    newFromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs)
        <$> (IO.newIOTypedArray (length vs) :: IO (IOTypedArray (Vector n t)))
    {-# INLINE newFromArray #-}
    newFromArray arr = coerce <$> (IO.newFromArray arr :: IO (IOTypedArray t))
    {-# INLINE index #-}
    index i arr = return . coerce $ js_indexVecArray arr i (dim (undefined :: Vector n t))
    {-# INLINE setIndex #-}
    setIndex i vec arr = js_setVecArray i (coerce vec) arr `seq` return ()
    {-# INLINE setList #-}
    setList i vecs arr = js_fillListJSArray i (unsafeCoerce $ seqList vecs) arr `seq` return ()
    {-# INLINE setArray #-}
    setArray i ar0 arr = IO.setArray (i* dim (undefined :: Vector n t)) ar0 (f arr)
        where f :: SomeTypedArray m (Vector n t) -> SomeTypedArray m t
              f = coerce

instance ( ST.STTypedArrayOperations t
         , KnownNat n
         ) => ST.STTypedArrayOperations (Vector n t) where
    {-# INLINE newSTTypedArray #-}
    newSTTypedArray n = coerce
        <$> (ST.newSTTypedArray $ n * dim (undefined :: Vector n t) :: ST.ST s (STTypedArray s t))
    {-# INLINE fillNewSTTypedArray #-}
    fillNewSTTypedArray n v = js_fillJSArray (coerce v) n
        <$> (ST.newSTTypedArray n :: ST.ST s (STTypedArray s (Vector n t)))
    {-# INLINE newFromList #-}
    newFromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs)
        <$> (ST.newSTTypedArray (length vs) :: ST.ST  s (STTypedArray s (Vector n t)))
    {-# INLINE newFromArray #-}
    newFromArray arr = coerce <$> (ST.newFromArray arr :: ST.ST s (STTypedArray s t))
    {-# INLINE index #-}
    index i arr = return . coerce $ js_indexVecArray arr i (dim (undefined :: Vector n t))
    {-# INLINE setIndex #-}
    setIndex i vec arr = js_setVecArray i (coerce vec) arr `seq` return ()
    {-# INLINE setList #-}
    setList i vecs arr = js_fillListJSArray i (unsafeCoerce $ seqList vecs) arr `seq` return ()
    {-# INLINE setArray #-}
    setArray i ar0 arr = ST.setArray (i* dim (undefined :: Vector n t)) ar0 (f arr)
        where f :: SomeTypedArray m (Vector n t) -> SomeTypedArray m t
              f = coerce












instance ( ImmutableArrayBufferPrim (TypedArray t)
         ) => ImmutableArrayBufferPrim (TypedArray (Matrix n t)) where
    {-# INLINE fromByteArrayPrim #-}
    fromByteArrayPrim ba = coerce (fromByteArrayPrim ba :: TypedArray t)
    {-# INLINE toByteArrayPrim #-}
    toByteArrayPrim arr = toByteArrayPrim (coerce arr :: TypedArray t)

instance ( MutableArrayBufferPrim (SomeTypedArray m t)
         ) => MutableArrayBufferPrim (SomeTypedArray m (Matrix n t)) where
    {-# INLINE fromMutableByteArrayPrim #-}
    fromMutableByteArrayPrim mba s = case fromMutableByteArrayPrim mba s of
        (# s1, arr0 :: SomeTypedArray m t #) -> (# s1, coerce arr0 #)
    {-# INLINE toMutableByteArrayPrim #-}
    toMutableByteArrayPrim arr = toMutableByteArrayPrim (coerce arr :: SomeTypedArray m t)

instance ( KnownNat n
         ) => ArrayBufferData (SomeTypedArray m (Matrix n t)) where
    {-# INLINE byteLength #-}
    byteLength arr = byteLength (coerce arr :: SomeTypedArray m t)
    {-# INLINE sliceImmutable #-}
    sliceImmutable i0 Nothing arr = coerce $ sliceImmutable
        (i0* m*m)
        Nothing (coerce arr :: SomeTypedArray m t)
        where m = dim (undefined :: Matrix n t)
    sliceImmutable i0 (Just i1) arr = coerce $ sliceImmutable
        (i0*m*m) (Just $ i1*m*m) (coerce arr :: SomeTypedArray m t)
        where m = dim (undefined :: Matrix n t)

instance ( TypedArrayOperations t
         , KnownNat n
         ) => TypedArrayOperations (Matrix n t) where
    {-# INLINE typedArray #-}
    typedArray n = coerce (typedArray $ n * m * m:: TypedArray t)
        where m = dim (undefined :: Matrix n t)
    {-# INLINE fillNewTypedArray #-}
    fillNewTypedArray n v = js_fillJSArray (coerce v) n (typedArray n :: TypedArray (Matrix n t))
    {-# INLINE fromList #-}
    fromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs) ar0
        where ar0 = typedArray (length vs) :: TypedArray (Matrix n t)
    {-# INLINE fromArray #-}
    fromArray arr = coerce (fromArray arr :: TypedArray t)
    {-# INLINE arrayView #-}
    arrayView buf = f $ arrayView buf
        where f :: SomeTypedArray m t -> SomeTypedArray m (Matrix n t)
              f = coerce
    {-# INLINE (!) #-}
    arr ! i = coerce $ js_indexVecArray arr i (m*m)
        where m = dim (undefined :: Matrix n t)
    {-# INLINE elemSize #-}
    elemSize _ = elemSize (undefined :: TypedArray t) * m*m
        where m = dim (undefined :: Matrix n t)
    {-# INLINE indexOf #-}
    indexOf = undefined
    {-# INLINE lastIndexOf #-}
    lastIndexOf = undefined

instance ( IO.IOTypedArrayOperations t
         , KnownNat n
         ) => IO.IOTypedArrayOperations (Matrix n t) where
    {-# INLINE newIOTypedArray #-}
    newIOTypedArray n = coerce
        <$> (IO.newIOTypedArray $ n * m*m :: IO (IOTypedArray t))
        where m = dim (undefined :: Matrix n t)
    {-# INLINE fillNewIOTypedArray #-}
    fillNewIOTypedArray n v = js_fillJSArray (coerce v) n
        <$> (IO.newIOTypedArray n :: IO (IOTypedArray (Matrix n t)))
    {-# INLINE newFromList #-}
    newFromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs)
        <$> (IO.newIOTypedArray (length vs) :: IO (IOTypedArray (Matrix n t)))
    {-# INLINE newFromArray #-}
    newFromArray arr = coerce <$> (IO.newFromArray arr :: IO (IOTypedArray t))
    {-# INLINE index #-}
    index i arr = return . coerce $ js_indexVecArray arr i (m*m)
        where m = dim (undefined :: Matrix n t)
    {-# INLINE setIndex #-}
    setIndex i vec arr = js_setVecArray i (coerce vec) arr `seq` return ()
    {-# INLINE setList #-}
    setList i vecs arr = js_fillListJSArray i (unsafeCoerce $ seqList vecs) arr `seq` return ()
    {-# INLINE setArray #-}
    setArray i ar0 arr = IO.setArray (i* m*m) ar0 (f arr)
        where f :: SomeTypedArray m (Matrix n t) -> SomeTypedArray m t
              f = coerce
              m = dim (undefined :: Matrix n t)

instance ( ST.STTypedArrayOperations t
         , KnownNat n
         ) => ST.STTypedArrayOperations (Matrix n t) where
    {-# INLINE newSTTypedArray #-}
    newSTTypedArray n = coerce
        <$> (ST.newSTTypedArray $ n * m*m:: ST.ST s (STTypedArray s t))
        where m = dim (undefined :: Matrix n t)
    {-# INLINE fillNewSTTypedArray #-}
    fillNewSTTypedArray n v = js_fillJSArray (coerce v) n
        <$> (ST.newSTTypedArray n :: ST.ST s (STTypedArray s (Matrix n t)))
    {-# INLINE newFromList #-}
    newFromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs)
        <$> (ST.newSTTypedArray (length vs) :: ST.ST  s (STTypedArray s (Matrix n t)))
    {-# INLINE newFromArray #-}
    newFromArray arr = coerce <$> (ST.newFromArray arr :: ST.ST s (STTypedArray s t))
    {-# INLINE index #-}
    index i arr = return . coerce $ js_indexVecArray arr i (m*m)
        where m = dim (undefined :: Matrix n t)
    {-# INLINE setIndex #-}
    setIndex i vec arr = js_setVecArray i (coerce vec) arr `seq` return ()
    {-# INLINE setList #-}
    setList i vecs arr = js_fillListJSArray i (unsafeCoerce $ seqList vecs) arr `seq` return ()
    {-# INLINE setArray #-}
    setArray i ar0 arr = ST.setArray (i* m*m) ar0 (f arr)
        where f :: SomeTypedArray m (Matrix n t) -> SomeTypedArray m t
              f = coerce
              m = dim (undefined :: Matrix n t)



seqList :: [a] -> [a]
seqList xs = foldr' seq () xs `seq` xs
