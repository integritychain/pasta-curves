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
supporting functionality such as point/element arithmetic, serialization, and 
hash-to-curve. The algorithms are NOT constant time.

\[
\text{Pallas: } y^2 = x^3 + 5 \text{ over } F_p(0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001)
\]

\[
\text{Vesta: }  y^2 = x^3 + 5 \text{ over } F_q(0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001)
\]

-}

{-# LANGUAGE DataKinds, NoImplicitPrelude, Safe #-}

module PastaCurves (Fp, Fq, Pallas, Vesta, CurvePt(..), Curve(..), hashToPallas, hashToVesta,
  Field(..), pallasPrime, vestaPrime, exampleFp, exampleFq, examplePallas, exampleVesta) where

import Pasta (Fp, Fq, Pallas, Vesta, CurvePt(..), Curve(..), Field(..), hashToPallas, hashToVesta, pallasPrime, vestaPrime)

{-}
ghci> :m + Data.ByteString.UTF8
ghci> hashToPallas (fromString "Trans rights now!")
-}

-- | An example `Fp` element (9).
exampleFp :: Fp
exampleFp = 9


-- | An example `Fq` element (8).
exampleFq :: Fq
exampleFq = 8


-- | An example `Pallas` point (base * 8).
examplePallas :: Pallas
examplePallas = pointMul exampleFq base


-- | An example `Vesta` point (base * 9).
exampleVesta :: Vesta
exampleVesta = pointMul exampleFp base
