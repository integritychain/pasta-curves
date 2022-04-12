{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, Safe #-}

module Main (main) where

import PastaCurves (projectName)
import Protolude


main :: IO ()
main = do
         print ("Executable for " :: Text)
         print projectName
