{-# LANGUAGE Trustworthy, ImportQualifiedPost, NoImplicitPrelude #-}

module Main (main) where

import Prelude (IO, print, ($))
import System.Environment (setEnv)
import Test.Tasty (defaultMain, testGroup)
import TestFields (fieldProps, testH2Fp)
import TestCurves (curveProps, testPOI)


main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "1_000"
  defaultMain $ testGroup "\nRunning Tests" [fieldProps, testH2Fp, curveProps, testPOI]
  print "wogga!"
