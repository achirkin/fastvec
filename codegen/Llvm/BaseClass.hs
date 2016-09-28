{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Llvm.BaseClass
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Llvm.BaseClass
  ( LlvmType (..), LlvmCode (..)
  , Index (..), mkVar, lTypVal, retCall
  , Pointer (..), pValUndef
  , Labeled (..)
  , LlvmArgTypes (..), LlvmArgs (..)
  , stackAlign
  ) where

#include "MachDeps.h"


import Data.List (intercalate)
import Data.Monoid
import GHC.TypeLits


stackAlign :: Int
stackAlign = SIZEOF_HSWORD

mkVar :: Index -> String
mkVar (OpIndex i) = "%" <> show i
mkVar (ArgIndex i)  = "%x" <> show i




data Index = OpIndex Int | ArgIndex Int
  deriving (Eq, Ord)

newtype Labeled a (s::Symbol) = Labeled { unLabel :: a }

instance (KnownSymbol s, LlvmType a) => LlvmType (Labeled a s) where
  ltype (Labeled a) = ltype a
  ltypeL x@(Labeled a) = ltypeL a <> " " <> symbolVal x
  lsize (Labeled a) = lsize a

instance (KnownSymbol s, LlvmCode a) => LlvmCode (Labeled a s) where
  lcode (Labeled a) = lcode a
  lvar = Labeled . lvar
  undef = Labeled undef




data Pointer t = Pointer Index | PUndef

pValUndef :: Pointer a -> a
pValUndef _ = undefined

instance LlvmType a => LlvmType (Pointer a) where
  ltype _ = ltype (undefined :: a) <> "*"
  lsize _ = stackAlign
instance LlvmType a => LlvmCode (Pointer a) where
  lcode PUndef = "undef"
  lcode (Pointer i) = mkVar i
  lvar = Pointer
  undef = PUndef




class LlvmType a where
  -- | try never use argument here
  ltype :: a -> String
  ltypeL :: a -> String
  ltypeL = ltype
  lsize :: a -> Int

class LlvmType a => LlvmCode a where
  lcode :: a -> String
  lvar  :: Index -> a
  undef :: a

class LlvmArgTypes a where
  largTypes :: a -> String

class LlvmArgTypes a => LlvmArgs a where
  largs :: a -> String
  largsL :: a -> String


lTypVal :: LlvmCode a => a -> String
lTypVal a = ltype a <> " " <> lcode a


lTypLV :: LlvmCode a => a -> String
lTypLV a = ltypeL a <> " " <> lcode a


instance LlvmType () where
  ltype _ = "void"
  lsize _ = stackAlign

instance LlvmCode () where
  lcode _ = "void"
  lvar _ = ()
  undef  = ()

nil :: a
nil = undefined

newtype S = S String

instance Show S where
  show (S s) = s


instance (LlvmArgTypes a, LlvmType r) => LlvmType (a -> r) where
  ltype _ = ltype r <> " " <> largTypes a
    where
      r = nil :: r
      a = nil :: a
  lsize _ = stackAlign


instance LlvmArgTypes () where
  largTypes _ = "()"
instance (LlvmType a, LlvmType b)
       => LlvmArgTypes (a,b) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b) = (nil,nil) :: (a,b)
      s = intercalate ", " [ltype a,ltype b]
instance (LlvmType a, LlvmType b, LlvmType c)
       => LlvmArgTypes (a,b,c) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c) = (nil,nil,nil) :: (a,b,c)
      s = intercalate ", " [ltype a,ltype b,ltype c]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d)
       => LlvmArgTypes (a,b,c,d) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d) = (nil,nil,nil,nil) :: (a,b,c,d)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e)
       => LlvmArgTypes (a,b,c,d,e) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e) = (nil,nil,nil,nil,nil) :: (a,b,c,d,e)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f)
       => LlvmArgTypes (a,b,c,d,e,f) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f) = (nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g)
       => LlvmArgTypes (a,b,c,d,e,f,g) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g) = (nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h)
       => LlvmArgTypes (a,b,c,d,e,f,g,h) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h) = (nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h, LlvmType i)
       => LlvmArgTypes (a,b,c,d,e,f,g,h,i) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h,i) = (nil,nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h,i)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h,ltype i]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h, LlvmType i, LlvmType j)
       => LlvmArgTypes (a,b,c,d,e,f,g,h,i,j) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h,i,j) = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h,i,j)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h,ltype i,ltype j]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h, LlvmType i, LlvmType j, LlvmType k)
       => LlvmArgTypes (a,b,c,d,e,f,g,h,i,j,k) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h,i,j,k) = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h,i,j,k)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h,ltype i,ltype j,ltype k]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h, LlvmType i, LlvmType j, LlvmType k, LlvmType l)
       => LlvmArgTypes (a,b,c,d,e,f,g,h,i,j,k,l) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h,i,j,k,l) = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h,i,j,k,l)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h,ltype i,ltype j,ltype k,ltype l]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h, LlvmType i, LlvmType j, LlvmType k, LlvmType l, LlvmType m)
       => LlvmArgTypes (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h,i,j,k,l,m) = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h,i,j,k,l,m)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h,ltype i,ltype j,ltype k,ltype l,ltype m]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h, LlvmType i, LlvmType j, LlvmType k, LlvmType l, LlvmType m, LlvmType n)
       => LlvmArgTypes (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h,ltype i,ltype j,ltype k,ltype l,ltype m,ltype n]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h, LlvmType i, LlvmType j, LlvmType k, LlvmType l, LlvmType m, LlvmType n, LlvmType o)
       => LlvmArgTypes (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h,ltype i,ltype j,ltype k,ltype l,ltype m,ltype n,ltype o]
instance (LlvmType a, LlvmType b, LlvmType c, LlvmType d, LlvmType e, LlvmType f, LlvmType g, LlvmType h, LlvmType i, LlvmType j, LlvmType k, LlvmType l, LlvmType m, LlvmType n, LlvmType o, LlvmType p)
       => LlvmArgTypes (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  largTypes _ = "(" <> s <> ")"
    where
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil) :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
      s = intercalate ", " [ltype a,ltype b,ltype c,ltype d,ltype e,ltype f,ltype g,ltype h,ltype i,ltype j,ltype k,ltype l,ltype m,ltype n,ltype o,ltype p]


instance LlvmArgs () where
  largs _ = "()"
  largsL _ = "()"
instance (LlvmCode a, LlvmCode b)
       => LlvmArgs (a,b) where
  largs (a,b) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b]
  largsL (a,b) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b]
instance (LlvmCode a, LlvmCode b, LlvmCode c)
       => LlvmArgs (a,b,c) where
  largs (a,b,c) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c]
  largsL (a,b,c) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d)
       => LlvmArgs (a,b,c,d) where
  largs (a,b,c,d) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d]
  largsL (a,b,c,d) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e)
       => LlvmArgs (a,b,c,d,e) where
  largs (a,b,c,d,e) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e]
  largsL (a,b,c,d,e) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f)
       => LlvmArgs (a,b,c,d,e,f) where
  largs (a,b,c,d,e,f) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f]
  largsL (a,b,c,d,e,f) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g)
       => LlvmArgs (a,b,c,d,e,f,g) where
  largs (a,b,c,d,e,f,g) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g]
  largsL (a,b,c,d,e,f,g) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h)
       => LlvmArgs (a,b,c,d,e,f,g,h) where
  largs (a,b,c,d,e,f,g,h) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h]
  largsL (a,b,c,d,e,f,g,h) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i)
       => LlvmArgs (a,b,c,d,e,f,g,h,i) where
  largs (a,b,c,d,e,f,g,h,i) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h,lTypVal i]
  largsL (a,b,c,d,e,f,g,h,i) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h,lTypLV i]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j)
       => LlvmArgs (a,b,c,d,e,f,g,h,i,j) where
  largs (a,b,c,d,e,f,g,h,i,j) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h,lTypVal i,lTypVal j]
  largsL (a,b,c,d,e,f,g,h,i,j) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h,lTypLV i,lTypLV j]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k)
       => LlvmArgs (a,b,c,d,e,f,g,h,i,j,k) where
  largs (a,b,c,d,e,f,g,h,i,j,k) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h,lTypVal i,lTypVal j,lTypVal k]
  largsL (a,b,c,d,e,f,g,h,i,j,k) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h,lTypLV i,lTypLV j,lTypLV k]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l)
       => LlvmArgs (a,b,c,d,e,f,g,h,i,j,k,l) where
  largs (a,b,c,d,e,f,g,h,i,j,k,l)  = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h,lTypVal i,lTypVal j,lTypVal k,lTypVal l]
  largsL (a,b,c,d,e,f,g,h,i,j,k,l) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h,lTypLV i,lTypLV j,lTypLV k,lTypLV l]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l, LlvmCode m)
       => LlvmArgs (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  largs (a,b,c,d,e,f,g,h,i,j,k,l,m) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h,lTypVal i,lTypVal j,lTypVal k,lTypVal l,lTypVal m]
  largsL (a,b,c,d,e,f,g,h,i,j,k,l,m) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h,lTypLV i,lTypLV j,lTypLV k,lTypLV l,lTypLV m]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l, LlvmCode m, LlvmCode n)
       => LlvmArgs (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  largs (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h,lTypVal i,lTypVal j,lTypVal k,lTypVal l,lTypVal m,lTypVal n]
  largsL (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h,lTypLV i,lTypLV j,lTypLV k,lTypLV l,lTypLV m,lTypLV n]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l, LlvmCode m, LlvmCode n, LlvmCode o)
       => LlvmArgs (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  largs (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h,lTypVal i,lTypVal j,lTypVal k,lTypVal l,lTypVal m,lTypVal n,lTypVal o]
  largsL (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h,lTypLV i,lTypLV j,lTypLV k,lTypLV l,lTypLV m,lTypLV n,lTypLV o]
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l, LlvmCode m, LlvmCode n, LlvmCode o, LlvmCode p)
       => LlvmArgs (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  largs (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypVal a,lTypVal b,lTypVal c,lTypVal d,lTypVal e,lTypVal f,lTypVal g,lTypVal h,lTypVal i,lTypVal j,lTypVal k,lTypVal l,lTypVal m,lTypVal n,lTypVal o,lTypVal p]
  largsL (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = "(" <> s <> ")"
    where
      s = intercalate ", " [lTypLV a,lTypLV b,lTypLV c,lTypLV d,lTypLV e,lTypLV f,lTypLV g,lTypLV h,lTypLV i,lTypLV j,lTypLV k,lTypLV l,lTypLV m,lTypLV n,lTypLV o,lTypLV p]



retCall :: (LlvmArgs a) => Pointer (a -> ()) -> a -> String
retCall fptr args = "tail call cc 10 void " <> lcode fptr <> largs args <> " nounwind"
