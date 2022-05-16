{-# LANGUAGE CPP, DataKinds, DerivingStrategies, FlexibleInstances, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, ScopedTypeVariables, Safe #-}
{-# LANGUAGE PolyKinds, TypeApplications #-}

module Pcurves (Curve(pointMul), CurvePt(base, fromBytes, negatePt, neutral, pointAdd, 
  toAffine, toBytes, toProjective), Point) where

import Prelude hiding (drop, length, sqrt)
import Data.ByteString (ByteString, cons, drop, index, length, pack)
import Fields (Field, Fz, fromBytes, inv0, shiftR1, sgn0, sqrt, toBytes)
import GHC.TypeLits (Nat, natVal, KnownNat)
import Data.Typeable (Proxy (Proxy))


data Point (a::Nat) (b::Nat) (baseX::Nat) (baseY::Nat) f = 
             Projective {_px :: f, _py :: f, _pz :: f} -- (x * inv0 z, y * inv0 z)
             | Affine {_ax :: f, _ay :: f}
             | PointAtInfinity deriving stock (Show)


#define PARAMS [natVal (Proxy :: Proxy a), natVal (Proxy :: Proxy b), natVal (Proxy :: Proxy baseX), natVal (Proxy :: Proxy baseY)]


instance (Field f, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) =>
  Eq (Point a b baseX baseY f) where
  (==) (Affine x1 y1) (Affine x2 y2) = (x1 == x2) && (y1 == y2)
  (==) PointAtInfinity PointAtInfinity = True
  (==) PointAtInfinity _ = False 
  (==) _ PointAtInfinity = False
  (==) pt1 pt2 = _toAffine pt1 == _toAffine pt2  -- one or both projective


class CurvePt a where
  base :: a
  fromBytes :: ByteString -> Maybe a
  negatePt :: a -> a
  neutral :: a
  pointAdd :: a -> a -> a
  toAffine :: a -> a
  toBytes :: a -> ByteString
  toProjective :: a -> a

instance (KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY, KnownNat z) => 
  CurvePt (Point a b baseX baseY (Fields.Fz z)) where
  base = Projective (fromInteger $ PARAMS !! 2) (fromInteger $ PARAMS !! 3) 1 
  fromBytes b = _fromBytes b (fromInteger $ PARAMS !! 0) (fromInteger $ PARAMS !! 1)
  negatePt pt = _negatePt pt
  neutral = Projective 0 1 0
  pointAdd pt1 pt2 = _pointAdd pt1 pt2 (fromInteger $ PARAMS !! 0) (fromInteger $ 3 * PARAMS !! 1)  -- b3=3*b
  toAffine pt = _toAffine pt
  toBytes pt = _toBytes pt
  toProjective pt = _toProjective pt


class (CurvePt a, Field b) => Curve a b where
  pointMul :: b -> a -> a


instance (KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY, KnownNat z1, 
  KnownNat z2) => Curve (Point a b baseX baseY (Fields.Fz z1)) (Fields.Fz z2) where
  pointMul s pt = _pointMul s pt (Projective 0 1 0) (fromInteger $ PARAMS !! 0) (fromInteger $ 3 * PARAMS !! 1) -- 0 15


_fromBytes :: (Field f) => ByteString -> f -> f -> Maybe (Point a b baseX baseY f)
_fromBytes bytes a b
    | length bytes == 1 && index bytes 0 == 0 = Just PointAtInfinity
    | length bytes == expLen && (index bytes 0 == 0x2 || index bytes 0 == 0x03) = result
        where
         expLen = 1 + length (Fields.toBytes a)
         x = (+ a*0) <$> Fields.fromBytes (drop 1 bytes)  -- a*0 forces type
         sgn0y = if index bytes 0 == 0x02 then 0 else 1::Integer
         alpha = (\t -> t^(3::Integer) + a * t + b) <$> x
         beta = alpha >>= sqrt
         y =  (\t -> if sgn0 t == sgn0y then t else negate t) <$> beta
         result = (Affine <$> x <*> y)
_fromBytes _ _ _ = Nothing


_negatePt :: (Field f, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) => 
  Point a b baseX baseY f -> Point a b baseX baseY f
_negatePt (Projective x y z) = Projective x (- y) z
_negatePt PointAtInfinity = PointAtInfinity
_negatePt a = _negatePt $ _toProjective a


-- See https://eprint.iacr.org/2015/1060.pdf page 8; The following has all the additions 'squashed out'
-- Algorithm 1: Complete, projective point addition for arbitrary prime order short Weierstrass curves E/Fq : y^2 = x^3 + ax + b.
_pointAdd :: (Field f, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) => 
  Point a b baseX baseY f -> Point a b baseX baseY f -> f -> f -> Point a b baseX baseY f
_pointAdd (Projective x1 y1 z1) (Projective x2 y2 z2) a b3 = result
  where
        m0 = x1 * x2
        m1 = y1 * y2
        m2 = z1 * z2
        m3 = (x1 + y1) * (x2 + y2)
        m4 = (x1 + z1) * (x2 + z2)
        m5 = (y1 + z1) * (y2 + z2)
        m6 = a * (- m0 - m2 + m4)
        m7 = b3 * m2
        m8 = (m1 - m6 - m7) * (m1 + m6 + m7)
        m9 = a * m2
        m10 = b3 * (- m0 - m2 + m4)
        m11 = a * (m0 - m9)
        m12 = (m0 * 3 + m9) * (m10 + m11)
        m13 = (- m1 - m2 + m5) * (m10 + m11)
        m14 = (- m0 - m1 + m3) * (m1 - m6 - m7)
        m15 = (- m0 - m1 + m3) * (m0 * 3 + m9)
        m16 = (- m1 - m2 + m5) * (m1 + m6 + m7)
        result = Projective (-m13 + m14) (m8 + m12) (m15 + m16)
_pointAdd PointAtInfinity pt2 _ _ = pt2
_pointAdd pt1 PointAtInfinity _ _ = pt1
_pointAdd pt1 pt2 a b3 = _pointAdd (_toProjective pt1) (_toProjective pt2) a b3


-- scalar * point, neutral, a, b3 -> point
_pointMul :: (Field f1, Field f2, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) => 
  f2 -> Point a b baseX baseY f1 -> Point a b baseX baseY f1 -> f1 -> f1 -> Point a b baseX baseY f1
_pointMul scalar pt accum a b3
  | scalar == 0 = accum
  | sgn0 scalar /= 0 = _pointMul (shiftR1 scalar) doublePt (_pointAdd accum pt a b3) a b3
  | sgn0 scalar == 0 = _pointMul (shiftR1 scalar) doublePt accum a b3
  | otherwise = error "_pointMul pattern match fail (should never happen)"
  where
    doublePt = _pointAdd pt pt a b3


_toAffine :: (Field f) => Point a b baseX baseY f -> Point a b baseX baseY f
_toAffine (Projective _ _ 0) = PointAtInfinity
_toAffine (Projective x y z) = Affine (x * inv0 z) (y * inv0 z)
_toAffine (Affine x y) = Affine x y
_toAffine PointAtInfinity = PointAtInfinity


-- Compressed; section 2.3.3 on page 10 of https://www.secg.org/sec1-v2.pdf
_toBytes :: (Field f, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) => 
  Point a b baseX baseY f -> ByteString
_toBytes PointAtInfinity = pack [0]
_toBytes (Affine x y)
  | sgn0 y == 0 = cons 0x02 (Fields.toBytes x)
  | otherwise   = cons 0x03 (Fields.toBytes x)
_toBytes pt = _toBytes (_toAffine pt)


-- _toProjective :: (Field t, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) => 
_toProjective :: (Field f) => Point a b baseX baseY f -> Point a b baseX baseY f
_toProjective (Projective x y z) = Projective x y z
_toProjective (Affine x y) = Projective x y 1
_toProjective PointAtInfinity = Projective 0 1 0
