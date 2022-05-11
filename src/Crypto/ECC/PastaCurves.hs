{- |
Copyright: (c) 2022 Eric Schorn
SPDX-License-Identifier: MIT
Maintainer: Eric Schorn <eschorn@integritychain.com>

See README for more info
-}

-- TODO
--   0. Ensure serdes is correct
--   1. Clean up existing code
--   2. Ensure full test coverage
--   3. Install test coverage reporting?



{-# LANGUAGE CPP, DataKinds, KindSignatures, ImportQualifiedPost, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, Trustworthy #-}


module PastaCurves (projectName, a, b, c, main11, pTestTemp) where

import Prelude (IO, String, print, (*), error)

import Constants (pallasPrime, vestaPrime)
import Fields -- (primeField, sqrt)
import Curves (Pallas, neutral)
import Pcurves qualified as PC
import Language.Haskell.TH -- (TypeQ, litT, numTyLit,strTyLit)


main11 :: IO ()
main11 = do
  print ("hello, world" :: String)
  print (sqrt a)
  print (sqrt b)
  print (neutral :: Pallas)


pTestTemp :: $(PC.pCurve 123 "Fp") --a0
pTestTemp = PC.testBase

z :: $(PC.xx "MyName" pallasPrime 55)
--z = PC.PointAtInfinity
z = error "asdf"

a :: $(primeField pallasPrime)
a = 9


b :: $(primeField vestaPrime)
b = 9


c :: $(primeField pallasPrime)
c = 123*123


projectName :: String
projectName = "pasta-curves"
