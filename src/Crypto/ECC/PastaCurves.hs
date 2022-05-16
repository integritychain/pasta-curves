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

{-# LANGUAGE StandaloneDeriving, CPP, DataKinds, KindSignatures, ImportQualifiedPost, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, Trustworthy #-}


module PastaCurves (projectName, a, b, c, main11, Fp, Pallas, Fq, Vesta, 
  PC.Curve(pointMul), PC.CurvePt(base, fromBytes, negatePt, neutral, pointAdd, 
  toAffine, toBytes, toProjective)) where

import Prelude (IO, String, print, (*))

import Fields qualified as F -- (primeField, sqrt)
import Pcurves qualified as PC
--import GHC.TypeLits (Nat)


main11 :: IO ()
main11 = do
  print ("hello, world" :: String)
  print (F.sqrt a)
  print (F.sqrt b)
  print (PC.neutral :: PastaCurves.Pallas)


type Fp  = F.Fz 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001
type Pallas = (PC.Point 0 5 1 0x248b4a5cf5ed6c83ac20560f9c8711ab92e13d27d60fb1aa7f5db6c93512d546 (PastaCurves.Fp))

type Fq = F.Fz 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001
type Vesta  = (PC.Point 0 5 1 0x26bc999156dd5194ec49b1c551768ab375785e7ce00906d13e0361674fd8959f (PastaCurves.Fq))

zobba = PC.base :: PastaCurves.Pallas

a :: PastaCurves.Fp -- $(primeField pallasPrime)
a = 9


b :: Fq -- $(primeField vestaPrime)
b = 9


c :: PastaCurves.Fp -- $(primeField pallasPrime)
c = 123*123


projectName :: String
projectName = "pasta-curves"
