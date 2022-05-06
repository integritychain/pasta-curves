{-# LANGUAGE ImportQualifiedPost, NoImplicitPrelude, OverloadedStrings, Safe #-}

module Main (main) where

import Prelude
import PastaCurves qualified as PC


main :: IO ()
main = do
  print ("Benchmarks for " :: String)
  print PC.projectName
