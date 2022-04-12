{-# LANGUAGE OverloadedStrings, Trustworthy, NoImplicitPrelude, ImportQualifiedPost #-}


module Main (main) where

import Protolude
import Test.Tasty qualified as TT
import TestFields -- as F
--import TestCurves -- as C
import System.Environment qualified as SE

import PastaCurves (projectName)


main :: IO ()
main = do
         SE.setEnv "TASTY_QUICKCHECK_TESTS" "200"
         TT.defaultMain $ TT.testGroup "\nRunning Tests" [fieldProps, testH2Fp] -- , curveProps]
         print projectName

