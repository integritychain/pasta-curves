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

TODO: Latex description of Pasta Curves!!

\[
f(a) = \frac{1}{2\pi i}\oint_\gamma \frac{f(z)}{z-a}\,\mathrm{d}z
\]

-}

{-# LANGUAGE DataKinds, NoImplicitPrelude, Safe #-}

module PastaCurves (Fp, Fq, Pallas, Vesta, Curve(..), CurvePt(..), Field(..), 
  exampleFp, exampleFq, examplePallas, exampleVesta) where


import Prelude hiding (sqrt)
import Curves (Curve(..), CurvePt(..), Point)
import Fields (Fz, Field(..))


-- | `Fp` is the field element used as a coordinate in the Pallas elliptic curve.
-- It can also be used as a scalar to multiply a point on the Vesta elliptic curve.
-- It is a type synonym for the `Fields.Fz` type, parameterized with the modulus.
type Fp  = Fz 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001


-- | `Pallas` represents a point on the Pallas elliptic curve using `Fp` coordinates.
-- The curve was designed to have the some order as the `Fq` element\'s modulus. It is
-- a type synonym for the `Curves.Point` type, parameterized with the curve\s @a@ and
-- @b@ values and the affine base point as @base_x@ and @base_y@.
type Pallas = (Point 0 5 1 0x248b4a5cf5ed6c83ac20560f9c8711ab92e13d27d60fb1aa7f5db6c93512d546 Fp)


-- | `Fq` is the field element used as a coordinate in the Vesta elliptic curve.
-- It can also be used as a scalar to multiply a point on the Pallas elliptic curve.
-- It is a type synonym for the `Fields.Fz` type, parameterized with the modulus.
type Fq = Fz 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001


-- | `Vesta` represents a point on the Vesta elliptic curve using `Fq` coordinates.
-- The curve was designed to have the some order as the `Fp` element\'s modulus.  It is
-- a type synonym for the `Curves.Point` type, parameterized with the curve\s @a@ and
-- @b@ values and the affine base point as @base_x@ and @base_y@.
type Vesta  = (Point 0 5 1 0x26bc999156dd5194ec49b1c551768ab375785e7ce00906d13e0361674fd8959f Fq)


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
