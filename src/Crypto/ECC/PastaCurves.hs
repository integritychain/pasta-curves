{-|
Module      : Crypto.PastaCurves.PastaCurves
Description : Supports the instantiation of parameterized elliptic curves.
Copyright   : (c) Eric Schorn, 2022
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental
Portability : GHC
SPDX-License-Identifier: MIT

This module provides the Pasta Curves consisting of: the `Pallas` curve and its `Fp`
field element, the `Vesta` curve  and its `Fq` field element, and a variety of 
supporting functionality such as point arithmetic, serialization, and hash-to-curve. 
The algorithms are NOT constant time.
-}

-- TODO
--   0. Further clean up
--   1. Finish implementation of field-to-curve
--   2. Documentation
--   3. Finalizate testing/coverage.

{-# LANGUAGE DataKinds, NoImplicitPrelude, Safe #-}

module PastaCurves (projectName, a, b, c, main11, Fp, Pallas, Fq, Vesta,
  Curve(..), CurvePt(..)) where


import Prelude hiding (sqrt)
import Curves (Curve(..), CurvePt(..), Point)
import Fields (Fz, Field(..))


type Fp  = Fz 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001
type Pallas = (Point 0 5 1 0x248b4a5cf5ed6c83ac20560f9c8711ab92e13d27d60fb1aa7f5db6c93512d546 Fp)

type Fq = Fz 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001
type Vesta  = (Point 0 5 1 0x26bc999156dd5194ec49b1c551768ab375785e7ce00906d13e0361674fd8959f Fq)


main11 :: IO ()
main11 = do
  print "hello, world"
  print (sqrt a)
  print (sqrt b)
  print (neutral :: PastaCurves.Pallas)


a :: Fp
a = 9


b :: Fq
b = 9


c :: Fp
c = 123*123


projectName :: String
projectName = "pasta-curves"
