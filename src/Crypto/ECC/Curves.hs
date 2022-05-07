{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, Trustworthy #-}

module Curves (Curve(pointMul), CurvePt(base, fromBytes, negatePt, neutral, pointAdd, 
  toAffine, toBytes, toProjective), Fp, Fq, Pallas, Vesta) where

import Prelude(Applicative((<*>)), Bool(..), Eq(..), Integer, Maybe(..), Monad((>>=)), 
  Num(..), Show, ($), (<$>), (^), (&&), (||), error, negate, otherwise)
import Data.ByteString (ByteString, cons, drop, index, length, pack)
import Fields (Field, fromBytes, inv0, primeField, shiftR1, sgn0, sqrt, toBytes)
import Constants (pallasPrime, vestaPrime)


data Point a = Projective {_px :: a, _py :: a, _pz :: a} -- (x * inv0 z, y * inv0 z)
               | Affine {_ax :: a, _ay :: a}
               | PointAtInfinity deriving stock (Show)


instance (Field a) => Eq (Point a) where
  (==) (Affine x1 y1) (Affine x2 y2) = (x1 == x2) && (y1 == y2)
  (==) PointAtInfinity PointAtInfinity = True
  (==) PointAtInfinity _ = False 
  (==) _ PointAtInfinity = False
  (==) pt1 pt2 = _toAffine pt1 == _toAffine pt2  -- one or more operand is projective


type Fp = $(primeField pallasPrime)
type Fq = $(primeField vestaPrime)
newtype Pallas = Pallas (Point Fp) deriving stock (Show, Eq)
newtype Vesta  = Vesta  (Point Fq) deriving stock (Show, Eq)


class CurvePt a where
  base :: a
  fromBytes :: ByteString -> Maybe a
  negatePt :: a -> a
  neutral :: a
  pointAdd :: a -> a -> a
  toAffine :: a -> a
  toBytes :: a -> ByteString
  toProjective :: a -> a


instance CurvePt Pallas where
  base = Pallas $ Projective 1 0x248b4a5cf5ed6c83ac20560f9c8711ab92e13d27d60fb1aa7f5db6c93512d546 1
  fromBytes b = Pallas <$> _fromBytes b 0 5
  negatePt (Pallas pt) = Pallas $ _negatePt pt
  neutral = Pallas $ Projective 0 1 0
  pointAdd (Pallas pt1) (Pallas pt2) = Pallas $ _pointAdd pt1 pt2 0 15  -- b3=3*b
  toAffine (Pallas pt) = Pallas $ _toAffine pt
  toBytes (Pallas pt) = _toBytes pt
  toProjective (Pallas pt) = Pallas $ _toProjective pt


instance CurvePt Vesta where
  base = Vesta $ Projective  1 0x26bc999156dd5194ec49b1c551768ab375785e7ce00906d13e0361674fd8959f 1
  fromBytes b = Vesta <$> _fromBytes b 0 5
  negatePt (Vesta pt) = Vesta $ _negatePt pt
  neutral = Vesta $ Projective 0 1 0
  pointAdd (Vesta pt1) (Vesta pt2) = Vesta $ _pointAdd pt1 pt2 0 15  -- b3=3*b
  toAffine (Vesta pt) = Vesta $ _toAffine pt
  toBytes (Vesta pt) = _toBytes pt
  toProjective (Vesta pt) = Vesta $ _toProjective pt


class (CurvePt a, Field b) => Curve a b where
  pointMul :: b -> a -> a


instance Curve Pallas Fq where
  pointMul s (Pallas pt) = Pallas $ _pointMul s pt (Projective 0 1 0) 0 15


instance Curve Vesta Fp where
  pointMul s (Vesta pt) = Vesta $ _pointMul s pt (Projective 0 1 0) 0 15


_fromBytes :: Field a => ByteString -> a -> a -> Maybe (Point a)
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
         result = Affine <$> x <*> y
_fromBytes _ _ _ = Nothing


_negatePt :: (Field a) => Point a -> Point a
_negatePt (Projective x y z) = Projective x (- y) z
_negatePt PointAtInfinity = PointAtInfinity
_negatePt a = _negatePt $ _toProjective a


-- See https://eprint.iacr.org/2015/1060.pdf page 8; The following has all the additions 'squashed out'
-- Algorithm 1: Complete, projective point addition for arbitrary prime order short Weierstrass curves E/Fq : y^2 = x^3 + ax + b.
_pointAdd :: (Field a) => Point a -> Point a -> a -> a -> Point a
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
_pointMul :: (Field a, Field b) => b -> Point a -> Point a -> a -> a -> Point a
_pointMul scalar pt accum a b3
  | scalar == 0 = accum
  | sgn0 scalar /= 0 = _pointMul (shiftR1 scalar) doublePt (_pointAdd accum pt a b3) a b3
  | sgn0 scalar == 0 = _pointMul (shiftR1 scalar) doublePt accum a b3
  | otherwise = error "_pointMul pattern match fail (should never happen)"
  where
    doublePt = _pointAdd pt pt a b3


_toAffine :: (Field a) => Point a -> Point a
_toAffine (Projective _ _ 0) = PointAtInfinity
_toAffine (Projective x y z) = Affine (x * inv0 z) (y * inv0 z)
_toAffine (Affine x y) = Affine x y
_toAffine PointAtInfinity = PointAtInfinity


-- Compressed; section 2.3.3 on page 10 of https://www.secg.org/sec1-v2.pdf
_toBytes :: Field a => Point a -> ByteString
_toBytes PointAtInfinity = pack [0]
_toBytes (Projective x y z) = _toBytes (_toAffine (Projective x y z))
_toBytes (Affine x y)
  | sgn0 y == 0 = cons 0x02 (Fields.toBytes x)
  | otherwise     = cons 0x03 (Fields.toBytes x)


_toProjective :: Field a => Point a -> Point a
_toProjective (Projective x y z) = Projective x y z
_toProjective (Affine x y) = Projective x y 1
_toProjective PointAtInfinity = Projective 0 1 0
