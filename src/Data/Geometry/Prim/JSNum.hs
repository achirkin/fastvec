{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Prim.JSNum
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Prim.JSNum
    where

import GHCJS.Prim
import GHCJS.Types
import Data.Int
import Data.Word
import Foreign.C.Types


{-# INLINE broadcastJSVec #-}
foreign import javascript unsafe "$r = broadcastJSVec($1,$2)"
    broadcastJSVec :: JSRef -> Int -> JSRef

class (Num a) => JSNum a where
    toNum :: JSRef -> a
    fromNum :: a -> JSRef


-- convert to / from JSRef
#define JSNUM(T) \
    foreign import javascript unsafe "$r = $1" \
        toNum/**/T :: JSRef -> T;  {-# INLINE toNum/**/T #-}; \
    foreign import javascript unsafe "$r = $1" \
        fromNum/**/T :: T -> JSRef;  {-# INLINE fromNum/**/T #-}; \
    instance JSNum T where { \
        toNum = toNum/**/T; {-# INLINE toNum #-}; \
        fromNum = fromNum/**/T; {-# INLINE fromNum #-}; }

JSNUM(Int)
JSNUM(Int32)
JSNUM(Int16)
JSNUM(Int8)
JSNUM(Word)
JSNUM(Word32)
JSNUM(Word16)
JSNUM(Word8)
JSNUM(Float)
JSNUM(Double)
JSNUM(CChar)
JSNUM(CSChar)
JSNUM(CUChar)
JSNUM(CShort)
JSNUM(CUShort)
JSNUM(CInt)
JSNUM(CUInt)
JSNUM(CLong)
JSNUM(CULong)
JSNUM(CFloat)
JSNUM(CDouble)

-- VectorMath

{-# INLINE eyeJSMat #-}
foreign import javascript unsafe "$r = eyeJSMat($1)"
    eyeJSMat :: Int -> JSRef

{-# INLINE diagJSMat #-}
foreign import javascript unsafe "$r = diagJSMat($1,$2)"
    diagJSMat :: JSRef -> Int -> JSRef

{-# INLINE transposeJSMat #-}
foreign import javascript unsafe "$r = transposeJSMat($1,$2) "
    transposeJSMat :: JSRef -> Int -> JSRef

{-# INLINE detJSMat #-}
foreign import javascript unsafe "$r = detJSMat($1,$2)"
    detJSMat :: JSRef -> Int -> JSRef

{-# INLINE traceJSMat #-}
foreign import javascript unsafe "$r = traceJSMat($1,$2)"
    traceJSMat :: JSRef -> Int -> JSRef

{-# INLINE fromDiagJSMat #-}
foreign import javascript unsafe "$r = fromDiagJSMat($1,$2)"
    fromDiagJSMat :: JSRef -> Int -> JSRef

{-# INLINE toDiagJSMat #-}
foreign import javascript unsafe "$r = toDiagJSMat($1)"
    toDiagJSMat :: JSRef -> JSRef

{-# INLINE dotJSVec #-}
foreign import javascript unsafe "$r = dotJSVec($1,$2)"
    dotJSVec :: JSRef -> JSRef -> JSRef

{-# INLINE dotBJSVec #-}
foreign import javascript unsafe "$r = dotBJSVec($1,$2)"
    dotBJSVec :: JSRef -> JSRef -> JSRef

-- MatrixProduct

{-# INLINE prodJSMM #-}
foreign import javascript unsafe "$r = prodJSMM($1,$2,$3)"
    prodJSMM :: JSRef -> JSRef -> Int -> JSRef

{-# INLINE prodJSMV #-}
foreign import javascript unsafe "$r = prodJSMV($1,$2)"
    prodJSMV :: JSRef -> JSRef -> JSRef

-- VectorFracMath

{-# INLINE inverseJSM4 #-}
foreign import javascript unsafe "$r = inverseJSM4($1)"
    inverseJSM4 :: JSRef -> JSRef

{-# INLINE inverseJSM3 #-}
foreign import javascript unsafe "$r = inverseJSM3($1)"
    inverseJSM3 :: JSRef -> JSRef

{-# INLINE inverseJSM2 #-}
foreign import javascript unsafe "$r = inverseJSM2($1)"
    inverseJSM2 :: JSRef -> JSRef


-- Show

{-# INLINE showJSVec #-}
foreign import javascript unsafe "$r = showJSVec($1)"
    showJSVec :: JSRef -> JSString

{-# INLINE showJSMat #-}
foreign import javascript unsafe "$r = showJSMat($1,$2)"
    showJSMat :: JSRef -> Int -> JSString

-- Eq

{-# INLINE eqJSVec #-}
foreign import javascript unsafe "$r = eqJSVec($1,$2)"
    eqJSVec :: JSRef -> JSRef -> Bool

{-# INLINE neqJSVec #-}
foreign import javascript unsafe "$r = neqJSVec($1,$2)"
    neqJSVec :: JSRef -> JSRef -> Bool

-- Ord

{-# INLINE gtJSVec #-}
foreign import javascript unsafe "$r = gtJSVec($1,$2)"
    gtJSVec :: JSRef -> JSRef -> Bool

{-# INLINE ltJSVec #-}
foreign import javascript unsafe "$r = ltJSVec($1,$2)"
    ltJSVec :: JSRef -> JSRef -> Bool

{-# INLINE geJSVec #-}
foreign import javascript unsafe "$r = geJSVec($1,$2)"
    geJSVec :: JSRef -> JSRef -> Bool

{-# INLINE leJSVec #-}
foreign import javascript unsafe "$r = leJSVec($1,$2)"
    leJSVec :: JSRef -> JSRef -> Bool

{-# INLINE maxJSVec #-}
foreign import javascript unsafe "$r = maxJSVec($1,$2)"
    maxJSVec :: JSRef -> JSRef -> JSRef

{-# INLINE minJSVec #-}
foreign import javascript unsafe "$r = minJSVec($1,$2)"
    minJSVec :: JSRef -> JSRef -> JSRef

{-# INLINE cmpJSVec #-}
foreign import javascript unsafe "$r = cmpJSVec($1,$2)"
    cmpJSVec :: JSRef -> JSRef -> Int


-- Num

{-# INLINE plusJSVec #-}
foreign import javascript unsafe "$r = plusJSVec($1,$2)"
    plusJSVec :: JSRef -> JSRef -> JSRef

{-# INLINE minusJSVec #-}
foreign import javascript unsafe "$r = minusJSVec($1,$2)"
    minusJSVec :: JSRef -> JSRef -> JSRef

{-# INLINE timesJSVec #-}
foreign import javascript unsafe "$r = timesJSVec($1,$2)"
    timesJSVec :: JSRef -> JSRef -> JSRef

{-# INLINE negateJSVec #-}
foreign import javascript unsafe "$r = negateJSVec($1)"
    negateJSVec :: JSRef -> JSRef

{-# INLINE absJSVec #-}
foreign import javascript unsafe "$r = absJSVec($1)"
    absJSVec :: JSRef -> JSRef

{-# INLINE signumJSVec #-}
foreign import javascript unsafe "$r = signumJSVec($1)"
    signumJSVec :: JSRef -> JSRef


-- Fractional

{-# INLINE divideJSVec #-}
foreign import javascript unsafe "$r = divideJSVec($1,$2)"
    divideJSVec :: JSRef -> JSRef -> JSRef

{-# INLINE recipJSVec #-}
foreign import javascript unsafe "$r = recipJSVec($1)"
    recipJSVec :: JSRef -> JSRef

-- Floating

{-# INLINE piJSVec #-}
foreign import javascript unsafe "$r = piJSVec($1)"
    piJSVec :: Int -> JSRef

{-# INLINE expJSVec #-}
foreign import javascript unsafe "$r = expJSVec($1)"
    expJSVec :: JSRef -> JSRef

{-# INLINE logJSVec #-}
foreign import javascript unsafe "$r = logJSVec($1)"
    logJSVec :: JSRef -> JSRef

{-# INLINE sqrtJSVec #-}
foreign import javascript unsafe "$r = sqrtJSVec($1)"
    sqrtJSVec :: JSRef -> JSRef

{-# INLINE powerJSVec #-}
foreign import javascript unsafe "$r = powerJSVec($1,$2)"
    powerJSVec :: JSRef -> JSRef -> JSRef

{-# INLINE sinJSVec #-}
foreign import javascript unsafe "$r = sinJSVec($1)"
    sinJSVec :: JSRef -> JSRef

{-# INLINE cosJSVec #-}
foreign import javascript unsafe "$r = cosJSVec($1)"
    cosJSVec :: JSRef -> JSRef

{-# INLINE tanJSVec #-}
foreign import javascript unsafe "$r = tanJSVec($1)"
    tanJSVec :: JSRef -> JSRef

{-# INLINE asinJSVec #-}
foreign import javascript unsafe "$r = asinJSVec($1)"
    asinJSVec :: JSRef -> JSRef

{-# INLINE acosJSVec #-}
foreign import javascript unsafe "$r = acosJSVec($1)"
    acosJSVec :: JSRef -> JSRef

{-# INLINE atanJSVec #-}
foreign import javascript unsafe "$r = atanJSVec($1)"
    atanJSVec :: JSRef -> JSRef

{-# INLINE sinhJSVec #-}
foreign import javascript unsafe "$r = sinhJSVec($1)"
    sinhJSVec :: JSRef -> JSRef

{-# INLINE coshJSVec #-}
foreign import javascript unsafe "$r = coshJSVec($1)"
    coshJSVec :: JSRef -> JSRef

{-# INLINE tanhJSVec #-}
foreign import javascript unsafe "$r = tanhJSVec($1)"
    tanhJSVec :: JSRef -> JSRef

{-# INLINE asinhJSVec #-}
foreign import javascript unsafe "$r = asinhJSVec($1)"
    asinhJSVec :: JSRef -> JSRef

{-# INLINE acoshJSVec #-}
foreign import javascript unsafe "$r = acoshJSVec($1)"
    acoshJSVec :: JSRef -> JSRef

{-# INLINE atanhJSVec #-}
foreign import javascript unsafe "$r = atanhJSVec($1)"
    atanhJSVec :: JSRef -> JSRef


-- Storable

{-# INLINE writeByteOffJSVecFloat32 #-}
foreign import javascript unsafe "writeByteOffJSVecFloat32($1,$2,$3)"
    writeByteOffJSVecFloat32 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeByteOffJSVecFloat64 #-}
foreign import javascript unsafe "writeByteOffJSVecFloat64($1,$2,$3)"
    writeByteOffJSVecFloat64 :: Ref# -> Int -> JSRef -> IO ()

{-# INLINE writeByteOffJSVecInt8 #-}
foreign import javascript unsafe "writeByteOffJSVecInt8($1,$2,$3)"
    writeByteOffJSVecInt8 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeByteOffJSVecInt16 #-}
foreign import javascript unsafe "writeByteOffJSVecInt16($1,$2,$3)"
    writeByteOffJSVecInt16 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeByteOffJSVecInt32 #-}
foreign import javascript unsafe "writeByteOffJSVecInt32($1,$2,$3)"
    writeByteOffJSVecInt32 :: Ref# -> Int -> JSRef -> IO ()

{-# INLINE writeByteOffJSVecUint8 #-}
foreign import javascript unsafe "writeByteOffJSVecUint8($1,$2,$3)"
    writeByteOffJSVecUint8 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeByteOffJSVecUint16 #-}
foreign import javascript unsafe "writeByteOffJSVecUint16($1,$2,$3)"
    writeByteOffJSVecUint16 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeByteOffJSVecUint32 #-}
foreign import javascript unsafe "writeByteOffJSVecUint32($1,$2,$3)"
    writeByteOffJSVecUint32 :: Ref# -> Int -> JSRef -> IO ()


{-# INLINE readByteOffJSVecFloat32 #-}
foreign import javascript unsafe "readByteOffJSVecFloat32($1,$2,$3)"
    readByteOffJSVecFloat32 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readByteOffJSVecFloat64 #-}
foreign import javascript unsafe "readByteOffJSVecFloat64($1,$2,$3)"
    readByteOffJSVecFloat64 :: Ref# -> Int -> Int -> JSRef

{-# INLINE readByteOffJSVecInt8 #-}
foreign import javascript unsafe "readByteOffJSVecInt8($1,$2,$3)"
    readByteOffJSVecInt8 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readByteOffJSVecInt16 #-}
foreign import javascript unsafe "readByteOffJSVecInt16($1,$2,$3)"
    readByteOffJSVecInt16 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readByteOffJSVecInt32 #-}
foreign import javascript unsafe "readByteOffJSVecInt32($1,$2,$3)"
    readByteOffJSVecInt32 :: Ref# -> Int -> Int -> JSRef

{-# INLINE readByteOffJSVecUint8 #-}
foreign import javascript unsafe "readByteOffJSVecUint8($1,$2,$3)"
    readByteOffJSVecUint8 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readByteOffJSVecUint16 #-}
foreign import javascript unsafe "readByteOffJSVecUint16($1,$2,$3)"
    readByteOffJSVecUint16 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readByteOffJSVecUint32 #-}
foreign import javascript unsafe "readByteOffJSVecUint32($1,$2,$3)"
    readByteOffJSVecUint32 :: Ref# -> Int -> Int -> JSRef


{-# INLINE writeElemOffJSVecFloat32 #-}
foreign import javascript unsafe "writeElemOffJSVecFloat32($1,$2,$3)"
    writeElemOffJSVecFloat32 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeElemOffJSVecFloat64 #-}
foreign import javascript unsafe "writeElemOffJSVecFloat64($1,$2,$3)"
    writeElemOffJSVecFloat64 :: Ref# -> Int -> JSRef -> IO ()

{-# INLINE writeElemOffJSVecInt8 #-}
foreign import javascript unsafe "writeElemOffJSVecInt8($1,$2,$3)"
    writeElemOffJSVecInt8 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeElemOffJSVecInt16 #-}
foreign import javascript unsafe "writeElemOffJSVecInt16($1,$2,$3)"
    writeElemOffJSVecInt16 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeElemOffJSVecInt32 #-}
foreign import javascript unsafe "writeElemOffJSVecInt32($1,$2,$3)"
    writeElemOffJSVecInt32 :: Ref# -> Int -> JSRef -> IO ()

{-# INLINE writeElemOffJSVecUint8 #-}
foreign import javascript unsafe "writeElemOffJSVecUint8($1,$2,$3)"
    writeElemOffJSVecUint8 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeElemOffJSVecUint16 #-}
foreign import javascript unsafe "writeElemOffJSVecUint16($1,$2,$3)"
    writeElemOffJSVecUint16 :: Ref# -> Int -> JSRef -> IO ()
{-# INLINE writeElemOffJSVecUint32 #-}
foreign import javascript unsafe "writeElemOffJSVecUint32($1,$2,$3)"
    writeElemOffJSVecUint32 :: Ref# -> Int -> JSRef -> IO ()


{-# INLINE readElemOffJSVecFloat32 #-}
foreign import javascript unsafe "readElemOffJSVecFloat32($1,$2,$3)"
    readElemOffJSVecFloat32 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readElemOffJSVecFloat64 #-}
foreign import javascript unsafe "readElemOffJSVecFloat64($1,$2,$3)"
    readElemOffJSVecFloat64 :: Ref# -> Int -> Int -> JSRef

{-# INLINE readElemOffJSVecInt8 #-}
foreign import javascript unsafe "readElemOffJSVecInt8($1,$2,$3)"
    readElemOffJSVecInt8 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readElemOffJSVecInt16 #-}
foreign import javascript unsafe "readElemOffJSVecInt16($1,$2,$3)"
    readElemOffJSVecInt16 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readElemOffJSVecInt32 #-}
foreign import javascript unsafe "readElemOffJSVecInt32($1,$2,$3)"
    readElemOffJSVecInt32 :: Ref# -> Int -> Int -> JSRef

{-# INLINE readElemOffJSVecUint8 #-}
foreign import javascript unsafe "readElemOffJSVecUint8($1,$2,$3)"
    readElemOffJSVecUint8 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readElemOffJSVecUint16 #-}
foreign import javascript unsafe "readElemOffJSVecUint16($1,$2,$3)"
    readElemOffJSVecUint16 :: Ref# -> Int -> Int -> JSRef
{-# INLINE readElemOffJSVecUint32 #-}
foreign import javascript unsafe "readElemOffJSVecUint32($1,$2,$3)"
    readElemOffJSVecUint32 :: Ref# -> Int -> Int -> JSRef



foreign import javascript unsafe "console.log($1)"
    printRef :: JSRef -> IO ()



foreign import javascript unsafe "$r = [$1,$2]"
    jsVector2 :: JSRef -> JSRef -> JSRef

foreign import javascript unsafe "$r = [$1,$2,$3]"
    jsVector3 :: JSRef -> JSRef -> JSRef -> JSRef

foreign import javascript unsafe "$r = [$1,$2,$3,$4]"
    jsVector4 :: JSRef -> JSRef -> JSRef -> JSRef -> JSRef

foreign import javascript unsafe "$r = $1.concat($2)"
    jsMatrix2 :: JSRef -> JSRef -> JSRef

foreign import javascript unsafe "$r = $1.concat($2,$3)"
    jsMatrix3 :: JSRef -> JSRef -> JSRef -> JSRef

foreign import javascript unsafe "$r = $1.concat($2,$3,$4)"
    jsMatrix4 :: JSRef -> JSRef -> JSRef -> JSRef -> JSRef
