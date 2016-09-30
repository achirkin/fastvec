{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where

import Language.Haskell.TH (runIO)
import Data.Geometry.Gen

$(runIO $ [] <$ writeAllFiles)

main :: IO ()
main = writeAllFiles
