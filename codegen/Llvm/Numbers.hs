{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Llvm.Numbers
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Llvm.Numbers
  ( NumberType (..), IsSigned (..), NumberCat (..)
  , NumberCategory, NumberIsSigned
  , LlvmNum (..), eBitWidth, eNumberType
  , Llvm.Numbers.isSigned, numberCat, eIsSigned, eNumberCat
  , LlvmFloating (..), LlvmIntegral (..)
  , Numbers (..), dim, broadcast
  , Float32, Float64
  , SInt01, SInt08, SInt16, SInt32, SInt64
  , UInt01, UInt08, UInt16, UInt32, UInt64
  , Float32X, Float64X
  , SInt01X, SInt08X, SInt16X, SInt32X, SInt64X
  , UInt01X, UInt08X, UInt16X, UInt32X, UInt64X
  , enforceBits, enforceSize
  , HsPointer, HsWord, HsInt
  ) where


#include "MachDeps.h"

--import Control.Monad
--import Control.Applicative
import GHC.TypeLits

--import qualified Data.IntSet as Set
--import Data.Text as T
import Data.Monoid
--import Data.IORef

import Data.Int
import Data.Word
import Data.Bits
import Unsafe.Coerce (unsafeCoerce)

import Llvm.BaseClass
import System.IO.Unsafe (unsafePerformIO)
import Foreign (malloc)
import Foreign.Storable (Storable(..))
import Foreign.C (CFloat (..), CDouble (..))


data NumberType = NT_FP | NT_INT | NT_WORD
data IsSigned   = Signed | Unsigned
data NumberCat  = CatFP | CatInt


type family NumberCategory (t::NumberType) where
  NumberCategory 'NT_FP   = 'CatFP
  NumberCategory 'NT_INT  = 'CatInt
  NumberCategory 'NT_WORD = 'CatInt


type family NumberIsSigned (t::NumberType) where
  NumberIsSigned 'NT_FP   = 'Signed
  NumberIsSigned 'NT_INT  = 'Signed
  NumberIsSigned 'NT_WORD = 'Unsigned


class (Eq (NVal t bits), Num (NVal t bits), Ord (NVal t bits), Show (NVal t bits))
    => LlvmNum (t::NumberType) (bits::Nat) where
  data NVal t bits
  bitWidth :: Numbers t bits n -> Int
  numberType :: Numbers t bits n -> NumberType

eBitWidth :: LlvmNum t bits => NVal t bits -> Int
eBitWidth = bitWidth . NConst

eNumberType :: LlvmNum t bits => NVal t bits -> NumberType
eNumberType = numberType . NConst

isSigned :: LlvmNum t bits => Numbers t bits n -> Bool
isSigned x = case numberType x of
  NT_FP -> True
  NT_INT -> True
  NT_WORD -> False

eIsSigned :: LlvmNum t bits => NVal t bits ->  Bool
eIsSigned = Llvm.Numbers.isSigned . NConst

numberCat :: LlvmNum t bits => Numbers t bits n -> NumberCat
numberCat x = case numberType x of
  NT_FP -> CatFP
  NT_INT -> CatInt
  NT_WORD -> CatInt

eNumberCat :: LlvmNum t bits => NVal t bits -> NumberCat
eNumberCat = numberCat . NConst

class ( Eq (NVal 'NT_FP bits)
      , Ord (NVal 'NT_FP bits)
      , RealFrac (NVal 'NT_FP bits)
      , RealFloat (NVal 'NT_FP bits)
      , Fractional (NVal 'NT_FP bits)
      , Num (NVal 'NT_FP bits)
      , Real (NVal 'NT_FP bits)
      , Floating (NVal 'NT_FP bits)
      , Show (NVal 'NT_FP bits)
      , LlvmNum 'NT_FP bits
      , NFloating bits ~ NVal 'NT_FP bits
      ) => LlvmFloating (bits::Nat) where
  type NFloating bits
  type FNumbers bits (n::Nat)
  coerceToInt  :: NVal 'NT_FP bits -> NVal 'NT_INT bits
  coerceToWord :: NVal 'NT_FP bits -> NVal 'NT_WORD bits
--  bitsMinusZero :: NVal 'NT_INT bits
--  bitsOne       :: NVal 'NT_INT bits



class ( Eq (NIntegral s bits)
      , Ord (NIntegral s bits)
      , Bits (NIntegral s bits)
      , FiniteBits (NIntegral s bits)
      , Bounded (NIntegral s bits)
      , Num (NIntegral s bits)
      , Enum (NIntegral s bits)
      , Real (NIntegral s bits)
      , Integral (NIntegral s bits)
      , Show (NIntegral s bits)
      , Eq (NVal 'NT_INT bits)
      , Ord (NVal 'NT_INT bits)
      , Bits (NVal 'NT_INT bits)
      , FiniteBits (NVal 'NT_INT bits)
      , Bounded (NVal 'NT_INT bits)
      , Num (NVal 'NT_INT bits)
      , Enum (NVal 'NT_INT bits)
      , Real (NVal 'NT_INT bits)
      , Integral (NVal 'NT_INT bits)
      , Show (NVal 'NT_INT bits)
      , Eq (NVal 'NT_WORD bits)
      , Ord (NVal 'NT_WORD bits)
      , Bits (NVal 'NT_WORD bits)
      , FiniteBits (NVal 'NT_WORD bits)
      , Bounded (NVal 'NT_WORD bits)
      , Num (NVal 'NT_WORD bits)
      , Enum (NVal 'NT_WORD bits)
      , Real (NVal 'NT_WORD bits)
      , Integral (NVal 'NT_WORD bits)
      , Show (NVal 'NT_WORD bits)
      , LlvmNum 'NT_INT bits
      , LlvmNum 'NT_WORD bits
      ) => LlvmIntegral (s::IsSigned) (bits::Nat) where
  type NIntegral s bits
  type INumbers s bits (n::Nat)





-- | All numeric types in LLVM IR at once
data Numbers (t::NumberType) (bits::Nat) (n::Nat) where
  -- | undef in LLVM
  NUndef :: Numbers t bits n
  -- | Single-valued constant
  NConst :: LlvmNum t bits => NVal t bits -> Numbers t bits 1
  -- | Variable with dynamic value
  NVar   :: Index -> Numbers t bits n
  -- | Construct higher-dimensional number
  (:$)   :: Numbers t bits 1 -> Numbers t bits (n-1) -> Numbers t bits n
infixr 5 :$




-- | Dimensionality of the vector
dim :: KnownNat n => Numbers t bits n -> Int
dim = fromInteger . natVal

broadcast :: KnownNat n => Numbers t bits 1 -> Numbers t bits n
broadcast x = rez
  where
    rez = broadcast' (dim rez) x

broadcast' :: Int -> Numbers t bits 1  -> Numbers t bits n
broadcast' 0 x = unsafeCoerce x
broadcast' 1 x = unsafeCoerce x
broadcast' n x = x :$ broadcast' (n-1) x


instance Eq (Numbers t bits n) where
  NConst a == NConst b = a == b
  NVar i   == NVar j   = i == j
  (a:$u)   == (b:$v)   = a == b && u == v
  _        == _        = False

instance Ord (Numbers t bits n) where
  compare = cmpNumbers

cmpNumbers :: Numbers t bits n -> Numbers t bits n -> Ordering
NUndef `cmpNumbers` NUndef = EQ
NUndef `cmpNumbers` _ = LT
_      `cmpNumbers` NUndef = GT
NConst x `cmpNumbers` NConst y = compare x y
NConst _ `cmpNumbers` NVar _ = LT
NConst _ `cmpNumbers` (_:$_) = LT
NVar _ `cmpNumbers` NConst _ = GT
NVar i `cmpNumbers` NVar j   = compare i j
NVar _ `cmpNumbers` (_:$_)   = LT
(_:$_) `cmpNumbers` NConst _ = GT
(_:$_) `cmpNumbers` NVar   _ = GT
(x:$u) `cmpNumbers` (y:$v)   = cmpNumbers x y <> cmpNumbers u v
























instance LlvmNum 'NT_FP 32 where
  newtype NVal 'NT_FP 32 = F32 Float
    deriving (Eq,Ord,Real,RealFrac,RealFloat,Num,Fractional,Floating)
  bitWidth _ = 32
  numberType _ = NT_FP
instance LlvmNum 'NT_FP 64 where
  newtype NVal 'NT_FP 64 = F64 Double
    deriving (Eq,Ord,Real,RealFrac,RealFloat,Num,Fractional,Floating)
  bitWidth _ = 64
  numberType _ = NT_FP


instance LlvmNum 'NT_INT 1 where
  newtype NVal 'NT_INT 1 = I1 Bool
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Enum)
  bitWidth _ = 1
  numberType _ = NT_INT
instance LlvmNum 'NT_INT 8 where
  newtype NVal 'NT_INT 8 = I8 Int8
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Num,Enum,Real,Integral)
  bitWidth _ = 8
  numberType _ = NT_INT
instance LlvmNum 'NT_INT 16 where
  newtype NVal 'NT_INT 16 = I16 Int16
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Num,Enum,Real,Integral)
  bitWidth _ = 16
  numberType _ = NT_INT
instance LlvmNum 'NT_INT 32 where
  newtype NVal 'NT_INT 32 = I32 Int32
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Num,Enum,Real,Integral)
  bitWidth _ = 32
  numberType _ = NT_INT
instance LlvmNum 'NT_INT 64 where
  newtype NVal 'NT_INT 64 = I64 Int64
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Num,Enum,Real,Integral)
  bitWidth _ = 64
  numberType _ = NT_INT

instance LlvmNum 'NT_WORD 1 where
  newtype NVal 'NT_WORD 1 = W1 Bool
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Enum)
  bitWidth _ = 1
  numberType _ = NT_WORD
instance LlvmNum 'NT_WORD 8 where
  newtype NVal 'NT_WORD 8 = W8 Word8
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Num,Enum,Real,Integral)
  bitWidth _ = 8
  numberType _ = NT_WORD
instance LlvmNum 'NT_WORD 16 where
  newtype NVal 'NT_WORD 16 = W16 Word16
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Num,Enum,Real,Integral)
  bitWidth _ = 16
  numberType _ = NT_WORD
instance LlvmNum 'NT_WORD 32 where
  newtype NVal 'NT_WORD 32 = W32 Word32
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Num,Enum,Real,Integral)
  bitWidth _ = 32
  numberType _ = NT_WORD
instance LlvmNum 'NT_WORD 64 where
  newtype NVal 'NT_WORD 64 = W64 Word64
    deriving (Eq,Ord,Bits,FiniteBits,Bounded,Num,Enum,Real,Integral)
  bitWidth _ = 64
  numberType _ = NT_WORD

instance Show (NVal 'NT_FP 32) where
  show (F32 x) = show x
instance Show (NVal 'NT_FP 64) where
  show (F64 x) = show x


instance Show (NVal 'NT_INT 1) where
  show (I1 False) = "0"
  show (I1 True)  = "1"
instance Show (NVal 'NT_INT 8) where
  show (I8 x) = show x
instance Show (NVal 'NT_INT 16) where
  show (I16 x) = show x
instance Show (NVal 'NT_INT 32) where
  show (I32 x) = show x
instance Show (NVal 'NT_INT 64) where
  show (I64 x) = show x

instance Show (NVal 'NT_WORD 1) where
  show (W1 False) = "0"
  show (W1 True)  = "1"
instance Show (NVal 'NT_WORD 8) where
  show (W8 x) = show x
instance Show (NVal 'NT_WORD 16) where
  show (W16 x) = show x
instance Show (NVal 'NT_WORD 32) where
  show (W32 x) = show x
instance Show (NVal 'NT_WORD 64) where
  show (W64 x) = show x


instance Num (NVal 'NT_INT 1) where
  I1 x + I1 y = I1 $ x /= y
  I1 x * I1 y = I1 $ x && y
  abs = id
  signum = id
  negate (I1 x) = I1 $ not x
  fromInteger i = I1 $ i /= 0
instance Num (NVal 'NT_WORD 1) where
  W1 x + W1 y = W1 $ x /= y
  W1 x * W1 y = W1 $ x && y
  abs = id
  signum = id
  negate (W1 x) = W1 $ not x
  fromInteger i = W1 $ i /= 0

instance Real (NVal 'NT_INT 1) where
  toRational (I1 False) = 0
  toRational (I1 True) = 1
instance Real (NVal 'NT_WORD 1) where
  toRational (W1 False) = 0
  toRational (W1 True) = 1

instance Integral (NVal 'NT_INT 1) where
  toInteger (I1 False) = 0
  toInteger (I1 True) = 1
  quotRem a (I1 True) = (a , 0)
  quotRem _ (I1 False) = error "divide by zero"
instance Integral (NVal 'NT_WORD 1) where
  toInteger (W1 False) = 0
  toInteger (W1 True) = 1
  quotRem a (W1 True) = (a , 0)
  quotRem _ (W1 False) = error "divide by zero"


instance LlvmFloating 32 where
  type NFloating 32 = NVal 'NT_FP 32
  type FNumbers 32 (n::Nat) = Numbers 'NT_FP 32 n
  coerceToInt (F32 v) = unsafePerformIO $ do
     vptr <- malloc
     pokeByteOff vptr 0 (CFloat v)
     I32 <$> peek vptr
  coerceToWord (F32 v) = unsafePerformIO $ do
     vptr <- malloc
     pokeByteOff vptr 0 (CFloat v)
     W32 <$> peek vptr
--  bitsMinusZero = -2147483648
--  bitsOne = 1065353216

instance LlvmFloating 64 where
  type NFloating 64 = NVal 'NT_FP 64
  type FNumbers 64 (n::Nat) = Numbers 'NT_FP 64 n
  coerceToInt (F64 v) = unsafePerformIO $ do
     vptr <- malloc
     pokeByteOff vptr 0 (CDouble v)
     I64 <$> peek vptr
  coerceToWord (F64 v) = unsafePerformIO $ do
     vptr <- malloc
     pokeByteOff vptr 0 (CDouble v)
     W64 <$> peek vptr
--  bitsMinusZero = -9223372036854775808
--  bitsOne = 4607182418800017408


instance LlvmIntegral 'Signed 1 where
  type NIntegral 'Signed 1 = NVal 'NT_INT 1
  type INumbers 'Signed 1 (n::Nat) = Numbers 'NT_INT 1 n
instance LlvmIntegral 'Signed 8 where
  type NIntegral 'Signed 8 = NVal 'NT_INT 8
  type INumbers 'Signed 8 (n::Nat) = Numbers 'NT_INT 8 n
instance LlvmIntegral 'Signed 16 where
  type NIntegral 'Signed 16 = NVal 'NT_INT 16
  type INumbers 'Signed 16 (n::Nat) = Numbers 'NT_INT 16 n
instance LlvmIntegral 'Signed 32 where
  type NIntegral 'Signed 32 = NVal 'NT_INT 32
  type INumbers 'Signed 32 (n::Nat) = Numbers 'NT_INT 32 n
instance LlvmIntegral 'Signed 64 where
  type NIntegral 'Signed 64 = NVal 'NT_INT 64
  type INumbers 'Signed 64 (n::Nat) = Numbers 'NT_INT 64 n

instance LlvmIntegral 'Unsigned 1 where
  type NIntegral 'Unsigned 1 = NVal 'NT_WORD 1
  type INumbers 'Unsigned 1 (n::Nat) = Numbers 'NT_WORD 1 n
instance LlvmIntegral 'Unsigned 8 where
  type NIntegral 'Unsigned 8 = NVal 'NT_WORD 8
  type INumbers 'Unsigned 8 (n::Nat) = Numbers 'NT_WORD 8 n
instance LlvmIntegral 'Unsigned 16 where
  type NIntegral 'Unsigned 16 = NVal 'NT_WORD 16
  type INumbers 'Unsigned 16 (n::Nat) = Numbers 'NT_WORD 16 n
instance LlvmIntegral 'Unsigned 32 where
  type NIntegral 'Unsigned 32 = NVal 'NT_WORD 32
  type INumbers 'Unsigned 32 (n::Nat) = Numbers 'NT_WORD 32 n
instance LlvmIntegral 'Unsigned 64 where
  type NIntegral 'Unsigned 64 = NVal 'NT_WORD 64
  type INumbers 'Unsigned 64 (n::Nat) = Numbers 'NT_WORD 64 n


-- | Helper to enforce bitsize of a number
enforceBits :: NVal t bits -> NVal s bits -> NVal s bits
enforceBits = const id

-- | Helper to enforce bitsize and verctorsize of numbers
enforceSize :: Numbers t bits n -> Numbers s bits n -> Numbers s bits n
enforceSize = const id

instance (LlvmNum t bits, KnownNat n) => LlvmType (Numbers t bits n) where
  ltype v = case dim v of
    1 -> showBaseType v
    n -> "<" <> show n <> " x " <> showBaseType v <> ">"
  lsize v = max (lsize (undef :: Pointer (Numbers t bits n))) (bitWidth v * dim v)
instance (LlvmNum t bits, KnownNat n) => LlvmCode (Numbers t bits n) where
  lcode NUndef     = "undef"
  lcode (NConst x) = show x
  lcode (NVar i)   = mkVar i
  lcode v@(_:$_)   = "<" <> showVecContent v <> ">"
  lvar = NVar
  undef = NUndef


showBaseType :: LlvmNum t bits => Numbers t bits n -> String
showBaseType v | CatFP  <- numberCat v, 32 <- bitWidth v = "float"
               | CatFP  <- numberCat v, 64 <- bitWidth v = "double"
               | CatInt <- numberCat v                   = "i" <> show (bitWidth v)
               | otherwise                               = "f" <> show (bitWidth v)

showVecContent :: LlvmNum t bits => Numbers t bits n -> String
showVecContent v@NUndef = showBaseType v <> " undef"
showVecContent v@(NConst _) = lTypVal v
showVecContent v@(NVar i) = showBaseType v <> " " <> mkVar i
showVecContent (x:$w) = lTypVal x <> ", " <> showVecContent w







type Float32 = NVal 'NT_FP 32
type Float64 = NVal 'NT_FP 64

type SInt01 = NVal 'NT_INT 1
type SInt08 = NVal 'NT_INT 8
type SInt16 = NVal 'NT_INT 16
type SInt32 = NVal 'NT_INT 32
type SInt64 = NVal 'NT_INT 64

type UInt01 = NVal 'NT_WORD 1
type UInt08 = NVal 'NT_WORD 8
type UInt16 = NVal 'NT_WORD 16
type UInt32 = NVal 'NT_WORD 32
type UInt64 = NVal 'NT_WORD 64

type Float32X = Numbers 'NT_FP 32
type Float64X = Numbers 'NT_FP 64


type SInt01X = Numbers 'NT_INT 1
type SInt08X = Numbers 'NT_INT 8
type SInt16X = Numbers 'NT_INT 16
type SInt32X = Numbers 'NT_INT 32
type SInt64X = Numbers 'NT_INT 64

type UInt01X = Numbers 'NT_WORD 1
type UInt08X = Numbers 'NT_WORD 8
type UInt16X = Numbers 'NT_WORD 16
type UInt32X = Numbers 'NT_WORD 32
type UInt64X = Numbers 'NT_WORD 64


type HsPointer = Labeled (Pointer HsWord) "noalias nocapture"
type HsWord = Numbers 'NT_WORD
  WORD_SIZE_IN_BITS 1
type HsInt = Numbers 'NT_INT
  WORD_SIZE_IN_BITS 1
