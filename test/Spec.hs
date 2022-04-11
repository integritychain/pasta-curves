module Main (main) where

import qualified Test.Tasty as TT
import TestFields -- as F
--import TestCurves -- as C
import qualified System.Environment as SE

import PastaCurves (projectName)


main :: IO ()
main = do
         -- SE.setEnv "TASTY_QUICKCHECK_TESTS" "10_000"
         TT.defaultMain $ TT.testGroup "\nRunning Tests" [fieldProps, testH2Fp] -- , curveProps]

