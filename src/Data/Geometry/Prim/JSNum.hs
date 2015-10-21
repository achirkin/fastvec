{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples, PolyKinds #-}
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

import GHC.Exts as Exts

import JavaScript.TypedArray

import GHCJS.Prim
import GHCJS.Types
import Data.Int
import Data.Word
import Foreign.C.Types


{-# INLINE broadcastJSVec #-}
foreign import javascript unsafe "$r = broadcastJSVec($1,$2)"
    broadcastJSVec :: JSVal -> Int -> JSVal

class (Num a) => JSNum a where
    toNum :: JSVal -> a
    fromNum :: a -> JSVal


-- convert to / from JSVal
#define JSNUM(T) \
    foreign import javascript unsafe "$r = $1" \
        toNum/**/T :: JSVal -> T;  {-# INLINE toNum/**/T #-}; \
    foreign import javascript unsafe "$r = $1" \
        fromNum/**/T :: T -> JSVal;  {-# INLINE fromNum/**/T #-}; \
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
    eyeJSMat :: Int -> JSVal

{-# INLINE diagJSMat #-}
foreign import javascript unsafe "$r = diagJSMat($1,$2)"
    diagJSMat :: JSVal -> Int -> JSVal

{-# INLINE transposeJSMat #-}
foreign import javascript unsafe "$r = transposeJSMat($1,$2) "
    transposeJSMat :: JSVal -> Int -> JSVal

{-# INLINE detJSMat #-}
foreign import javascript unsafe "$r = detJSMat($1,$2)"
    detJSMat :: JSVal -> Int -> JSVal

{-# INLINE traceJSMat #-}
foreign import javascript unsafe "$r = traceJSMat($1,$2)"
    traceJSMat :: JSVal -> Int -> JSVal

{-# INLINE fromDiagJSMat #-}
foreign import javascript unsafe "$r = fromDiagJSMat($1,$2)"
    fromDiagJSMat :: JSVal -> Int -> JSVal

{-# INLINE toDiagJSMat #-}
foreign import javascript unsafe "$r = toDiagJSMat($1)"
    toDiagJSMat :: JSVal -> JSVal

{-# INLINE dotJSVec #-}
foreign import javascript unsafe "$r = dotJSVec($1,$2)"
    dotJSVec :: JSVal -> JSVal -> JSVal

{-# INLINE dotBJSVec #-}
foreign import javascript unsafe "$r = dotBJSVec($1,$2)"
    dotBJSVec :: JSVal -> JSVal -> JSVal


{-# INLINE indexJSVec #-}
foreign import javascript unsafe "$r = $2[$1]"
    indexJSVec :: Int -> JSVal -> JSVal

{-# INLINE unpackJSVec4 #-}
foreign import javascript unsafe "$r1 = $1[0]; $r2 = $1[1]; $r3 = $1[2]; $r4 = $1[3];"
    unpackJSVec4 :: JSVal -> (# JSVal,JSVal,JSVal,JSVal #)

{-# INLINE unpackJSVec3 #-}
foreign import javascript unsafe "$r1 = $1[0]; $r2 = $1[1]; $r3 = $1[2];"
    unpackJSVec3 :: JSVal -> (# JSVal,JSVal,JSVal #)

{-# INLINE unpackJSVec2 #-}
foreign import javascript unsafe "$r1 = $1[0]; $r2 = $1[1];"
    unpackJSVec2 :: JSVal -> (# JSVal,JSVal #)

{-# INLINE matColsJS #-}
foreign import javascript unsafe "$r = matColsJS($1,$2)"
    matColsJS :: JSVal -> Int -> JSVal

{-# INLINE matRowsJS #-}
foreign import javascript unsafe "$r = matRowsJS($1,$2)"
    matRowsJS :: JSVal -> Int -> JSVal

{-# INLINE resizeJSVec #-}
foreign import javascript unsafe "$r = resizeJSVec($1,$2)"
    resizeJSVec :: JSVal -> Int -> JSVal

{-# INLINE resizeJSMat #-}
foreign import javascript unsafe "$r = resizeJSMat($1,$2,$3)"
    resizeJSMat :: JSVal -> Int -> Int -> JSVal

-- MatrixProduct

{-# INLINE prodJSMM #-}
foreign import javascript unsafe "$r = prodJSMM($1,$2,$3)"
    prodJSMM :: JSVal -> JSVal -> Int -> JSVal

{-# INLINE prodJSMV #-}
foreign import javascript unsafe "$r = prodJSMV($1,$2)"
    prodJSMV :: JSVal -> JSVal -> JSVal

-- VectorFracMath

{-# INLINE inverseJSM4 #-}
foreign import javascript unsafe "$r = inverseJSM4($1)"
    inverseJSM4 :: JSVal -> JSVal

{-# INLINE inverseJSM3 #-}
foreign import javascript unsafe "$r = inverseJSM3($1)"
    inverseJSM3 :: JSVal -> JSVal

{-# INLINE inverseJSM2 #-}
foreign import javascript unsafe "$r = inverseJSM2($1)"
    inverseJSM2 :: JSVal -> JSVal


-- Show

{-# INLINE showJSVec #-}
foreign import javascript unsafe "$r = showJSVec($1)"
    showJSVec :: JSVal -> JSString

{-# INLINE showJSMat #-}
foreign import javascript unsafe "$r = showJSMat($1,$2)"
    showJSMat :: JSVal -> Int -> JSString

-- Eq

{-# INLINE eqJSVec #-}
foreign import javascript unsafe "$r = eqJSVec($1,$2)"
    eqJSVec :: JSVal -> JSVal -> Bool

{-# INLINE neqJSVec #-}
foreign import javascript unsafe "$r = neqJSVec($1,$2)"
    neqJSVec :: JSVal -> JSVal -> Bool

-- Ord

{-# INLINE gtJSVec #-}
foreign import javascript unsafe "$r = gtJSVec($1,$2)"
    gtJSVec :: JSVal -> JSVal -> Bool

{-# INLINE ltJSVec #-}
foreign import javascript unsafe "$r = ltJSVec($1,$2)"
    ltJSVec :: JSVal -> JSVal -> Bool

{-# INLINE geJSVec #-}
foreign import javascript unsafe "$r = geJSVec($1,$2)"
    geJSVec :: JSVal -> JSVal -> Bool

{-# INLINE leJSVec #-}
foreign import javascript unsafe "$r = leJSVec($1,$2)"
    leJSVec :: JSVal -> JSVal -> Bool

{-# INLINE maxJSVec #-}
foreign import javascript unsafe "$r = maxJSVec($1,$2)"
    maxJSVec :: JSVal -> JSVal -> JSVal

{-# INLINE minJSVec #-}
foreign import javascript unsafe "$r = minJSVec($1,$2)"
    minJSVec :: JSVal -> JSVal -> JSVal

{-# INLINE cmpJSVec #-}
foreign import javascript unsafe "$r = cmpJSVec($1,$2)"
    cmpJSVec :: JSVal -> JSVal -> Int


-- Num

{-# INLINE plusJSVec #-}
foreign import javascript unsafe "$r = plusJSVec($1,$2)"
    plusJSVec :: JSVal -> JSVal -> JSVal

{-# INLINE minusJSVec #-}
foreign import javascript unsafe "$r = minusJSVec($1,$2)"
    minusJSVec :: JSVal -> JSVal -> JSVal

{-# INLINE timesJSVec #-}
foreign import javascript unsafe "$r = timesJSVec($1,$2)"
    timesJSVec :: JSVal -> JSVal -> JSVal

{-# INLINE negateJSVec #-}
foreign import javascript unsafe "$r = negateJSVec($1)"
    negateJSVec :: JSVal -> JSVal

{-# INLINE absJSVec #-}
foreign import javascript unsafe "$r = absJSVec($1)"
    absJSVec :: JSVal -> JSVal

{-# INLINE signumJSVec #-}
foreign import javascript unsafe "$r = signumJSVec($1)"
    signumJSVec :: JSVal -> JSVal


-- Fractional

{-# INLINE divideJSVec #-}
foreign import javascript unsafe "$r = divideJSVec($1,$2)"
    divideJSVec :: JSVal -> JSVal -> JSVal

{-# INLINE recipJSVec #-}
foreign import javascript unsafe "$r = recipJSVec($1)"
    recipJSVec :: JSVal -> JSVal

-- Floating

{-# INLINE piJSVec #-}
foreign import javascript unsafe "$r = piJSVec($1)"
    piJSVec :: Int -> JSVal

{-# INLINE expJSVec #-}
foreign import javascript unsafe "$r = expJSVec($1)"
    expJSVec :: JSVal -> JSVal

{-# INLINE logJSVec #-}
foreign import javascript unsafe "$r = logJSVec($1)"
    logJSVec :: JSVal -> JSVal

{-# INLINE sqrtJSVec #-}
foreign import javascript unsafe "$r = sqrtJSVec($1)"
    sqrtJSVec :: JSVal -> JSVal

{-# INLINE powerJSVec #-}
foreign import javascript unsafe "$r = powerJSVec($1,$2)"
    powerJSVec :: JSVal -> JSVal -> JSVal

{-# INLINE sinJSVec #-}
foreign import javascript unsafe "$r = sinJSVec($1)"
    sinJSVec :: JSVal -> JSVal

{-# INLINE cosJSVec #-}
foreign import javascript unsafe "$r = cosJSVec($1)"
    cosJSVec :: JSVal -> JSVal

{-# INLINE tanJSVec #-}
foreign import javascript unsafe "$r = tanJSVec($1)"
    tanJSVec :: JSVal -> JSVal

{-# INLINE asinJSVec #-}
foreign import javascript unsafe "$r = asinJSVec($1)"
    asinJSVec :: JSVal -> JSVal

{-# INLINE acosJSVec #-}
foreign import javascript unsafe "$r = acosJSVec($1)"
    acosJSVec :: JSVal -> JSVal

{-# INLINE atanJSVec #-}
foreign import javascript unsafe "$r = atanJSVec($1)"
    atanJSVec :: JSVal -> JSVal

{-# INLINE sinhJSVec #-}
foreign import javascript unsafe "$r = sinhJSVec($1)"
    sinhJSVec :: JSVal -> JSVal

{-# INLINE coshJSVec #-}
foreign import javascript unsafe "$r = coshJSVec($1)"
    coshJSVec :: JSVal -> JSVal

{-# INLINE tanhJSVec #-}
foreign import javascript unsafe "$r = tanhJSVec($1)"
    tanhJSVec :: JSVal -> JSVal

{-# INLINE asinhJSVec #-}
foreign import javascript unsafe "$r = asinhJSVec($1)"
    asinhJSVec :: JSVal -> JSVal

{-# INLINE acoshJSVec #-}
foreign import javascript unsafe "$r = acoshJSVec($1)"
    acoshJSVec :: JSVal -> JSVal

{-# INLINE atanhJSVec #-}
foreign import javascript unsafe "$r = atanhJSVec($1)"
    atanhJSVec :: JSVal -> JSVal


-- Storable

{-# INLINE writeByteOffJSVecFloat32 #-}
foreign import javascript unsafe "writeByteOffJSVecFloat32($1,$2,$3)"
    writeByteOffJSVecFloat32 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeByteOffJSVecFloat64 #-}
foreign import javascript unsafe "writeByteOffJSVecFloat64($1,$2,$3)"
    writeByteOffJSVecFloat64 :: Ref# -> Int -> JSVal -> IO ()

{-# INLINE writeByteOffJSVecInt8 #-}
foreign import javascript unsafe "writeByteOffJSVecInt8($1,$2,$3)"
    writeByteOffJSVecInt8 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeByteOffJSVecInt16 #-}
foreign import javascript unsafe "writeByteOffJSVecInt16($1,$2,$3)"
    writeByteOffJSVecInt16 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeByteOffJSVecInt32 #-}
foreign import javascript unsafe "writeByteOffJSVecInt32($1,$2,$3)"
    writeByteOffJSVecInt32 :: Ref# -> Int -> JSVal -> IO ()

{-# INLINE writeByteOffJSVecUint8 #-}
foreign import javascript unsafe "writeByteOffJSVecUint8($1,$2,$3)"
    writeByteOffJSVecUint8 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeByteOffJSVecUint16 #-}
foreign import javascript unsafe "writeByteOffJSVecUint16($1,$2,$3)"
    writeByteOffJSVecUint16 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeByteOffJSVecUint32 #-}
foreign import javascript unsafe "writeByteOffJSVecUint32($1,$2,$3)"
    writeByteOffJSVecUint32 :: Ref# -> Int -> JSVal -> IO ()


{-# INLINE readByteOffJSVecFloat32 #-}
foreign import javascript unsafe "readByteOffJSVecFloat32($1,$2,$3)"
    readByteOffJSVecFloat32 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readByteOffJSVecFloat64 #-}
foreign import javascript unsafe "readByteOffJSVecFloat64($1,$2,$3)"
    readByteOffJSVecFloat64 :: Ref# -> Int -> Int -> JSVal

{-# INLINE readByteOffJSVecInt8 #-}
foreign import javascript unsafe "readByteOffJSVecInt8($1,$2,$3)"
    readByteOffJSVecInt8 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readByteOffJSVecInt16 #-}
foreign import javascript unsafe "readByteOffJSVecInt16($1,$2,$3)"
    readByteOffJSVecInt16 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readByteOffJSVecInt32 #-}
foreign import javascript unsafe "readByteOffJSVecInt32($1,$2,$3)"
    readByteOffJSVecInt32 :: Ref# -> Int -> Int -> JSVal

{-# INLINE readByteOffJSVecUint8 #-}
foreign import javascript unsafe "readByteOffJSVecUint8($1,$2,$3)"
    readByteOffJSVecUint8 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readByteOffJSVecUint16 #-}
foreign import javascript unsafe "readByteOffJSVecUint16($1,$2,$3)"
    readByteOffJSVecUint16 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readByteOffJSVecUint32 #-}
foreign import javascript unsafe "readByteOffJSVecUint32($1,$2,$3)"
    readByteOffJSVecUint32 :: Ref# -> Int -> Int -> JSVal


{-# INLINE writeElemOffJSVecFloat32 #-}
foreign import javascript unsafe "writeElemOffJSVecFloat32($1,$2,$3)"
    writeElemOffJSVecFloat32 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeElemOffJSVecFloat64 #-}
foreign import javascript unsafe "writeElemOffJSVecFloat64($1,$2,$3)"
    writeElemOffJSVecFloat64 :: Ref# -> Int -> JSVal -> IO ()

{-# INLINE writeElemOffJSVecInt8 #-}
foreign import javascript unsafe "writeElemOffJSVecInt8($1,$2,$3)"
    writeElemOffJSVecInt8 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeElemOffJSVecInt16 #-}
foreign import javascript unsafe "writeElemOffJSVecInt16($1,$2,$3)"
    writeElemOffJSVecInt16 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeElemOffJSVecInt32 #-}
foreign import javascript unsafe "writeElemOffJSVecInt32($1,$2,$3)"
    writeElemOffJSVecInt32 :: Ref# -> Int -> JSVal -> IO ()

{-# INLINE writeElemOffJSVecUint8 #-}
foreign import javascript unsafe "writeElemOffJSVecUint8($1,$2,$3)"
    writeElemOffJSVecUint8 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeElemOffJSVecUint16 #-}
foreign import javascript unsafe "writeElemOffJSVecUint16($1,$2,$3)"
    writeElemOffJSVecUint16 :: Ref# -> Int -> JSVal -> IO ()
{-# INLINE writeElemOffJSVecUint32 #-}
foreign import javascript unsafe "writeElemOffJSVecUint32($1,$2,$3)"
    writeElemOffJSVecUint32 :: Ref# -> Int -> JSVal -> IO ()


{-# INLINE readElemOffJSVecFloat32 #-}
foreign import javascript unsafe "readElemOffJSVecFloat32($1,$2,$3)"
    readElemOffJSVecFloat32 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readElemOffJSVecFloat64 #-}
foreign import javascript unsafe "readElemOffJSVecFloat64($1,$2,$3)"
    readElemOffJSVecFloat64 :: Ref# -> Int -> Int -> JSVal

{-# INLINE readElemOffJSVecInt8 #-}
foreign import javascript unsafe "readElemOffJSVecInt8($1,$2,$3)"
    readElemOffJSVecInt8 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readElemOffJSVecInt16 #-}
foreign import javascript unsafe "readElemOffJSVecInt16($1,$2,$3)"
    readElemOffJSVecInt16 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readElemOffJSVecInt32 #-}
foreign import javascript unsafe "readElemOffJSVecInt32($1,$2,$3)"
    readElemOffJSVecInt32 :: Ref# -> Int -> Int -> JSVal

{-# INLINE readElemOffJSVecUint8 #-}
foreign import javascript unsafe "readElemOffJSVecUint8($1,$2,$3)"
    readElemOffJSVecUint8 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readElemOffJSVecUint16 #-}
foreign import javascript unsafe "readElemOffJSVecUint16($1,$2,$3)"
    readElemOffJSVecUint16 :: Ref# -> Int -> Int -> JSVal
{-# INLINE readElemOffJSVecUint32 #-}
foreign import javascript unsafe "readElemOffJSVecUint32($1,$2,$3)"
    readElemOffJSVecUint32 :: Ref# -> Int -> Int -> JSVal


-- TypedArrays

{-# INLINE js_fillJSArray #-}
foreign import javascript unsafe "fillTypedArray($1,$2,$3)"
    js_fillJSArray :: JSVal -> Int -> SomeTypedArray any t -> SomeTypedArray any t

{-# INLINE js_setVecArray #-}
foreign import javascript unsafe "setVecArray($1,$2,$3)"
    js_setVecArray :: Int -> JSVal -> SomeTypedArray any t -> SomeTypedArray any t

{-# INLINE js_fillListJSArray #-}
foreign import javascript unsafe "fillListArray($1,$2,$3)"
    js_fillListJSArray :: Int -> Exts.Any -> SomeTypedArray any t -> SomeTypedArray any t

{-# INLINE js_indexVecArray #-}
foreign import javascript unsafe "indexVecArray($1,$2,$3)"
    js_indexVecArray :: SomeTypedArray any t -> Int -> Int -> JSVal

--foreign import javascript unsafe "console.log($1)"
--    printRef :: JSVal -> IO ()

-- Integral

{-# INLINE js_quot #-}
foreign import javascript unsafe "$r = $1.map(function (e, i) { return (e / $2[i] | 0);})"
    js_quot :: JSVal -> JSVal -> JSVal

{-# INLINE js_rem #-}
foreign import javascript unsafe "$r = $1.map(function (e, i) { return e - (e / $2[i] | 0)*$2[i];})"
    js_rem :: JSVal -> JSVal -> JSVal

{-# INLINE js_quotRem #-}
foreign import javascript unsafe "$r1 = []; $r2 = []; $1.foreach(function (e, i) { var x = e / $2[i] | 0; $r1.push(x); $r2.push(e - x*$2[i]);})"
    js_quotRem :: JSVal -> JSVal -> (JSVal, JSVal)

{-# INLINE js_div #-}
foreign import javascript unsafe "$r = $1.map(function (e, i) { return Math.floor(e / $2[i]);})"
    js_div :: JSVal -> JSVal -> JSVal

{-# INLINE js_mod #-}
foreign import javascript unsafe "$r = $1.map(function (e, i) { return e - Math.floor(e / $2[i])*$2[i];})"
    js_mod :: JSVal -> JSVal -> JSVal

{-# INLINE js_divMod #-}
foreign import javascript unsafe "$r1 = []; $r2 = []; $1.foreach(function (e, i) { var x = Math.floor(e / $2[i]); $r1.push(x); $r2.push(e - x*$2[i]);})"
    js_divMod :: JSVal -> JSVal -> (JSVal, JSVal)

{-# INLINE js_gcdVec #-}
foreign import javascript unsafe "$r = gcdVec($1,$2)"
    js_gcdVec :: JSVal -> JSVal -> JSVal

{-# INLINE js_lcmVec #-}
foreign import javascript unsafe "$r = lcmVec($1,$2)"
    js_lcmVec :: JSVal -> JSVal -> JSVal


-- RealFrac

{-# INLINE js_truncate #-}
foreign import javascript unsafe "$1.map(Math.trunc)"
    js_truncate :: JSVal -> JSVal


{-# INLINE js_round #-}
foreign import javascript unsafe "$1.map(Math.round)"
    js_round :: JSVal -> JSVal

{-# INLINE js_ceiling #-}
foreign import javascript unsafe "$1.map(Math.ceil)"
    js_ceiling :: JSVal -> JSVal

{-# INLINE js_floor #-}
foreign import javascript unsafe "$1.map(Math.floor)"
    js_floor :: JSVal -> JSVal

{-# INLINE js_properFraction #-}
foreign import javascript unsafe "$r1 = []; $r2 = []; $1.foreach(function (e) { var x = e | 0; $r1.push(x); $r2.push(e - x);})"
    js_properFraction :: JSVal -> (JSVal, JSVal)


foreign import javascript unsafe "$r = [$1,$2]"
    jsVector2 :: JSVal -> JSVal -> JSVal

foreign import javascript unsafe "$r = [$1,$2,$3]"
    jsVector3 :: JSVal -> JSVal -> JSVal -> JSVal

foreign import javascript unsafe "$r = [$1,$2,$3,$4]"
    jsVector4 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal

foreign import javascript unsafe "$r = $1.concat($2)"
    jsMatrix2 :: JSVal -> JSVal -> JSVal

foreign import javascript unsafe "$r = $1.concat($2,$3)"
    jsMatrix3 :: JSVal -> JSVal -> JSVal -> JSVal

foreign import javascript unsafe "$r = $1.concat($2,$3,$4)"
    jsMatrix4 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal
