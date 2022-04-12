{-# LANGUAGE NoImplicitPrelude, Trustworthy #-}

module Main (main) where

import Protolude
import PastaCurves (projectName)


main :: IO ()
main = do
         print "Benchmarks for "
         print projectName
