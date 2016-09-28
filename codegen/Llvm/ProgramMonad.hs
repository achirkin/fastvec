{-# LANGUAGE MultiParamTypeClasses, RecursiveDo #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Llvm.ProgramMonad
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Llvm.ProgramMonad
  ( LlvmProgram (..), get, put, op, runL, eprog, L (..)
  , LlvmDefine (..)
  , tailcall, call
  , OrderedList (), combineDeps
  ) where


import Control.Arrow (first)
import Control.Monad.Fix
import Data.Monoid
import qualified Data.List as List
import Llvm.BaseClass



data Command = OpIdx Int | Proc

newtype Ord a => OrderedList a = OL { _unOL :: [a] }

-- | Export all dependency declarations
combineDeps :: [OrderedList String] -> String
combineDeps deps = unlines $ List.foldl' (\xs (OL ys) -> List.union xs ys) [] deps

infixr 5 !:
(!:) :: Ord a => a -> OrderedList a -> OrderedList a
x !: OL [] = OL [x]
x !: OL (y:ys) | y < x     = y !: (x !: OL ys)
               | y == x    = OL (y:ys)
               | otherwise = OL (x:y:ys)

data LlvmProgram = LlvmProgram
  { funDec  :: String
  , codeSeq :: [(Command,String)]
  , reqDecs :: OrderedList String
  , argI :: Int
  , opI  :: Int
  }

eprog :: LlvmProgram
eprog = LlvmProgram
  { funDec = ""
  , codeSeq = []
  , reqDecs = OL []
  , argI = 0
  , opI = 0
  }

setFunDeclaration :: LlvmArgs a => String -> a -> L ()
setFunDeclaration fname fpams = modState $ \p -> p {
  funDec = "define cc 10 void @" <> fname <> " " <> largsL fpams <> " alwaysinline nounwind align " <> show stackAlign <> " {" }


newtype L a = L { _unL :: LlvmProgram -> (a, LlvmProgram) }

get :: L LlvmProgram
get = L $ \s -> (s,s)

put :: LlvmProgram -> L ()
put s = L $ const ((),s)

modState :: (LlvmProgram -> LlvmProgram) -> L ()
modState f = L $ \s -> ((),f s)


instance Functor L where
  fmap f (L mf) = L $ first f . mf
instance Applicative L where
  pure x = L $ (,) x
  (L mf) <*> (L mx) = L $ \m -> let (x,m') = mx m
                                    (f,m'') = mf m'
                                in (f x, m'')
instance Monad L where
  return x = L $ (,) x
  (L mx) >>= mf = L $ \m -> let (x,m') = mx m
                                L f = mf x
                            in f m'
instance MonadFix L where
  mfix f = L $ \s -> let (x,s') = _unL (f x) s in (x,s')




tailcall :: (LlvmArgs a) => Pointer (a -> ()) -> a -> L ()
tailcall fptr args = do
  () <- op (retCall fptr args)
  op "ret void"

-- | Perform an operation given as a raw code text;
--   depending on a result type it creates a new variable or does not.
op :: LlvmCode a => String -> L a
op code = do
  p <- get
  let i = opI p + 1
      rez = lvar (OpIndex i)
  case ltype rez of
    "void" -> put p { codeSeq =  (Proc, code) : codeSeq p}
    _ -> put p { codeSeq = (OpIdx i, code) : codeSeq p, opI = i}
  return rez


-- | Declare and call arbitrary function defined by llvm
call :: (LlvmCode b, LlvmArgs a)  => String -> a -> L b
call name args = mdo
  rez <- op $ "tail call " <> ltype rez <> " @" <> name <> largs args <> " nounwind"
  modState $ \p -> p { reqDecs =
                        ("declare " <> ltype rez <> " @" <> name <> largTypes args <> " nounwind readnone"
                        ) !: reqDecs p
                     }
  return rez


arg :: LlvmCode a => L a
arg = do
  p <- get
  let i = argI p + 1
  put p { argI = i }
  return $ lvar (ArgIndex i)


runL :: LlvmProgram -> L a -> (String, OrderedList String)
runL p (L f) =
    (unlines $
          funDec prog
        : Prelude.map pprint (reverse $ codeSeq prog)
        ++ ["}"]
    , reqDecs prog
    )
  where
    (_,prog) = f p
    pprint (OpIdx i, t) = "  " <> mkVar (OpIndex i) <> " = " <> t
    pprint (Proc   , t) = "  " <> t

class LlvmDefine a where
  define :: String -> L a



instance (LlvmCode a, LlvmCode b)
       => LlvmDefine (a,b) where
  define fname = do
    a <- (,) <$> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c)
       => LlvmDefine (a,b,c) where
  define fname = do
    a <- (,,) <$> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d)
       => LlvmDefine (a,b,c,d) where
  define fname = do
    a <- (,,,) <$> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e)
       => LlvmDefine (a,b,c,d,e) where
  define fname = do
    a <- (,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f)
       => LlvmDefine (a,b,c,d,e,f) where
  define fname = do
    a <- (,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g)
       => LlvmDefine (a,b,c,d,e,f,g) where
  define fname = do
    a <- (,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h)
       => LlvmDefine (a,b,c,d,e,f,g,h) where
  define fname = do
    a <- (,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i)
       => LlvmDefine (a,b,c,d,e,f,g,h,i) where
  define fname = do
    a <- (,,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j)
       => LlvmDefine (a,b,c,d,e,f,g,h,i,j) where
  define fname = do
    a <- (,,,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k)
       => LlvmDefine (a,b,c,d,e,f,g,h,i,j,k) where
  define fname = do
    a <- (,,,,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l)
       => LlvmDefine (a,b,c,d,e,f,g,h,i,j,k,l) where
  define fname = do
    a <- (,,,,,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l, LlvmCode m)
       => LlvmDefine (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  define fname = do
    a <- (,,,,,,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l, LlvmCode m, LlvmCode n)
       => LlvmDefine (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  define fname = do
    a <- (,,,,,,,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l, LlvmCode m, LlvmCode n, LlvmCode o)
       => LlvmDefine (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  define fname = do
    a <- (,,,,,,,,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a
instance (LlvmCode a, LlvmCode b, LlvmCode c, LlvmCode d, LlvmCode e, LlvmCode f, LlvmCode g, LlvmCode h, LlvmCode i, LlvmCode j, LlvmCode k, LlvmCode l, LlvmCode m, LlvmCode n, LlvmCode o, LlvmCode p)
       => LlvmDefine (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  define fname = do
    a <- (,,,,,,,,,,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg
    setFunDeclaration fname a
    return a

