{-# LANGUAGE ImportQualifiedPost, NoImplicitPrelude, OverloadedStrings, Safe #-}

module Main (main) where

import Protolude
import PastaCurves qualified as PC


main :: IO ()
main = do
  print ("Executable for " :: Text)
  print PC.projectName
