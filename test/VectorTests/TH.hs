{-# LANGUAGE TemplateHaskell #-}
module VectorTests.TH (TestType (..), writeTests, aggregateTests) where

import Test.Framework
import Data.List (sortBy, groupBy, intersectBy, foldl1')
import Data.Function (on)
import Data.Geometry

import Language.Haskell.TH
import VectorTests.VectorGenerators ()
import Control.Monad (liftM,forM,join)
import Data.Maybe (catMaybes)
-- import Debug.Trace (traceShow)

-- | Type of tests
data TestType = VecT | MatT | VecMat

-- | All instances of this class are to be added automatically to HTF main test function
class MultitypeTestSuite name where
    multitypeTestSuite :: name -> TestSuite

-- | Write a single test, with specified type signature and name
writeTest :: (TypeQ -> Q [Dec]) -> (String, TypeQ, Integer, Name) -> Q [(Dec, Maybe (Name, Name))]
writeTest test (t, typeQ, di, typeName) = liftM
    (renameFuncSuf $ nameBase typeName ++ t ++ "X" ++ show di)
    (test typeQ)

-- | Append suffix to the names of the functions
renameFuncSuf :: String -> [Dec] -> [(Dec, Maybe (Name, Name))]
renameFuncSuf suffix = map f
    where appendSuf name = mkName $ nameBase name ++ suffix
          f (SigD name sig) = (SigD (appendSuf name) sig, Nothing )
          f (FunD name fun) = (FunD (appendSuf name) fun, Just (name, appendSuf name))
          f x = (x, Nothing)


-- | Write specified test on specified types
writeTests :: TestType -> [Name] -> (TypeQ -> Q [Dec]) -> Q [Dec]
writeTests ttype classes test = do
    locTH <- location
    -- Generating all tests
    (tests, mnames) <- liftM (>>= (\t@(dim,name) -> case ttype of
                        VecT -> [("Vec", makeVectorType t, dim, name)]
                        MatT -> [("Mat", makeMatrixType t, dim, name)]
                        VecMat -> [ ("Vec", makeVectorType t, dim, name)
                                  , ("Mat", makeMatrixType t, dim, name)]))
                        (getVectorMathTypes classes)
                >>= (liftM (unzip . join) . mapM (writeTest test))
    let -- name of the module
        moduleName = loc_module locTH
        -- Location of the splice in HTF format
        loc = VarE 'makeLoc
                   `AppE` (LitE . StringL $ loc_filename locTH)
                   `AppE` (LitE . IntegerL . fromIntegral . fst $ loc_start locTH)
        -- [(name of the original function, names of generated functions)]
        names =
          map (\xs -> (fst $ head xs, map snd xs))
          . groupBy (\(n1,_) (n2,_) -> nameBase n1 == nameBase n2)
          . sortBy (\(n1,_) (n2,_) -> nameBase n1 `compare` nameBase n2)
          $ catMaybes mnames
        -- make test suite
        suite :: Name -> ExpQ -> Q [Dec]
        suite name gnames = liftM (DataD [] dataName [] [NormalC dataCon []] [] : )
            [d| instance MultitypeTestSuite $(return $ ConT dataName) where
                    multitypeTestSuite _ = makeTestSuite
                        $(return . LitE . StringL
                        . ((moduleName ++ ":") ++)
                        . drop 5 $ nameBase name)
                      . map (\(gname,gfunc) -> makeQuickCheckTest gname $(return loc) gfunc)
                      $ $(gnames)
              |] where dataName = mkName $ "MTS_" ++ nameBase name
                       dataCon = mkName $ "MTS_" ++ nameBase name ++ "_val"
        -- generate pairs of function (string) names and functions theirselves (IO ())
        gennames :: String -> [Name] -> ExpQ
        gennames basename = return . ListE
            . map (\gn -> TupE [ LitE . StringL . drop (length basename) $ nameBase gn
                               , VarE 'qcAssertion `AppE` VarE gn] )
    suites <- mapM (\(n,gns) -> suite n (gennames (nameBase n) gns) ) names
    return $ concat suites ++ tests

-- | Aggregate all generated tests into list of test suites
aggregateTests :: ExpQ
aggregateTests = do
    ClassI _ instances <- reify ''MultitypeTestSuite
    liftM ListE . forM instances
          $ \(InstanceD _ (AppT _ (ConT name)) _)
            -> [e| multitypeTestSuite $(return . ConE . mkName $ nameBase name ++ "_val") |]



--liftM (renameFunc $ nameBase n) $

-- DataD Cxt Name [TyVarBndr] [Con] [Name]
-- DataD [] MTS2_1627463391 [] [NormalC MTS2_1627463392 []] []
-- $(return . mkName $ "MTS" ++ nameBase name)
----------------------------------------------------------------------------------------------------


getVectorMathTypes :: [Name] -> Q [(Integer, Name)]
getVectorMathTypes classes = do
    ClassI _ instances <- reify ''VectorMath
    let vtypes = sortTypes
            $ instances >>= getDimName
    types <- liftM (map ((,) 0) . foldl1' (intersectBy ( (==) `on` nameBase )
                            )) $ mapM findInstances classes
    return $ intersectBy ( (==) `on` (nameBase . snd) ) vtypes types
    where findInstances cname = do
            ClassI _ is <- reify cname
            return . sortBy (compare `on` nameBase)
                   $ is >>= getIName
          getIName (InstanceD _ (AppT _ (ConT tn)) _ ) = [tn]
          getIName _ = []
          getDimName (InstanceD _ (AppT (AppT _ (LitT (NumTyLit di))) (ConT typeName)) _) = [(di,typeName)]
          getDimName _ = []

makeMatrixType :: (Integer, Name) -> TypeQ
makeMatrixType (di, name) = pure $ ConT ''Matrix `AppT` LitT (NumTyLit di) `AppT` ConT name

makeVectorType :: (Integer, Name) -> TypeQ
makeVectorType (di, name) = pure $ ConT ''Vector `AppT` LitT (NumTyLit di) `AppT` ConT name



sortTypes :: [(Integer, Name)] -> [(Integer, Name)]
sortTypes = sortBy (
                \(p1, name1)
                 (p2, name2)
                -> compare p1 p2 `mappend` compare (nameBase name1) (nameBase name2)
            )
