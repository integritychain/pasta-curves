{-# LANGUAGE OverloadedStrings, Trustworthy, NoImplicitPrelude, ImportQualifiedPost #-}


module Main (main) where

import Protolude
import Test.Tasty qualified as TT
import TestFields qualified as F
import TestCurves qualified as C
import System.Environment qualified as SE

import PastaCurves (projectName)


main :: IO ()
main = do
  SE.setEnv "TASTY_QUICKCHECK_TESTS" "200"
  TT.defaultMain $ TT.testGroup "\nRunning Tests" [F.fieldProps, F.testH2Fp, C.curveProps]
  print projectName

