{-# LANGUAGE ImportQualifiedPost, NoImplicitPrelude, OverloadedStrings, Safe #-}

module Main (main) where

import Prelude
import PastaCurves qualified as PC


main :: IO ()
main = do
  print ("Executable for " :: String)
  print PC.projectName
  print PC.pTestTemp
