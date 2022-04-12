{- |
Copyright: (c) 2022 Eric Schorn
SPDX-License-Identifier: MIT
Maintainer: Eric Schorn <eschorn@integritychain.com>

See README for more info
-}
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, KindSignatures  #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, Trustworthy, CPP #-}
{-# OPTIONS_GHC -Wall #-}


module PastaCurves
    ( projectName, a, b, c, main11
    ) where

import Protolude hiding (sqrt)


import Constants
import Fields
import Curves

main11 :: IO ()
main11 = do
  print ("hello, world" :: Text)
  print (sqrt a)
  print (sqrt b)
  print (neutral :: Pallas)

a :: $(primeField pallasPrime)
a = 9

b :: $(primeField vestaPrime)
b = 9

c :: $(primeField pallasPrime)
c = 123*123


projectName :: Text
projectName = "pasta-curves"
