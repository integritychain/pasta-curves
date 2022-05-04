{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleInstances, ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}

module Curves (CurvePt(base, fromBytes, {-isOnCurve,-} negatePt, neutral, pointAdd, toAffine, toBytes, toProjective),
               Curve(pointMul), Fp, Fq, Pallas, Vesta) where

import Protolude
import Data.ByteString qualified as DBS
import Fields qualified as F
import Constants qualified as C


data Point a = Projective {_px :: a, _py :: a, _pz :: a} -- (x * inv0 z, y * inv0 z)
               | Affine {_ax :: a, _ay :: a}
               | PointAtInfinity deriving stock (Show)


instance (F.Field a) => Eq (Point a) where
  (==) (Affine x1 y1) (Affine x2 y2) = (x1 == x2) && (y1 == y2)
  (==) PointAtInfinity PointAtInfinity = True
  (==) PointAtInfinity _ = False 
  (==) _ PointAtInfinity = False
  (==) pt1 pt2 = _toAffine pt1 == _toAffine pt2  -- one or more operand is projective


type Fp = $(F.primeField C.pallasPrime)
type Fq = $(F.primeField C.vestaPrime)
newtype Pallas = Pallas (Point Fp) deriving stock (Show, Eq)
newtype Vesta  = Vesta  (Point Fq) deriving stock (Show, Eq)


class CurvePt a where
  base :: a
  fromBytes :: ByteString -> Maybe a
  -- isOnCurve :: a -> Bool
  negatePt :: a -> a
  neutral :: a
  pointAdd :: a -> a -> a
  toAffine :: a -> a
  toBytes :: a -> ByteString
  toProjective :: a -> a


instance CurvePt Pallas where
  base = Pallas $ Projective 1 0x248b4a5cf5ed6c83ac20560f9c8711ab92e13d27d60fb1aa7f5db6c93512d546 1
  fromBytes b = Pallas <$> _fromBytes b 0 5
  -- isOnCurve (Pallas pt) = _isOnCurve pt 0 5
  negatePt (Pallas pt) = Pallas $ _negatePt pt
  neutral = Pallas $ Projective 0 1 0
  pointAdd (Pallas pt1) (Pallas pt2) = Pallas $ _pointAdd pt1 pt2 0 15  -- b3=3*b
  toAffine (Pallas pt) = Pallas $ _toAffine pt
  toBytes (Pallas pt) = _toBytes pt
  toProjective (Pallas pt) = Pallas $ _toProjective pt


instance CurvePt Vesta where
  base = Vesta $ Projective  1 0x26bc999156dd5194ec49b1c551768ab375785e7ce00906d13e0361674fd8959f 1
  fromBytes b = Vesta <$> _fromBytes b 0 5
  -- isOnCurve (Vesta pt) = _isOnCurve pt 0 5
  negatePt (Vesta pt) = Vesta $ _negatePt pt
  neutral = Vesta $ Projective 0 1 0
  pointAdd (Vesta pt1) (Vesta pt2) = Vesta $ _pointAdd pt1 pt2 0 15  -- b3=3*b
  toAffine (Vesta pt) = Vesta $ _toAffine pt
  toBytes (Vesta pt) = _toBytes pt
  toProjective (Vesta pt) = Vesta $ _toProjective pt


class (CurvePt a, F.Field b) => Curve a b where
  pointMul :: b -> a -> a


instance Curve Pallas Fq where
  pointMul s (Pallas pt) = Pallas $ _pointMul s pt (Projective 0 1 0) 0 15


instance Curve Vesta Fp where
  pointMul s (Vesta pt) = Vesta $ _pointMul s pt (Projective 0 1 0) 0 15


_fromBytes :: F.Field a => ByteString -> a -> a -> Maybe (Point a)
_fromBytes bytes a b
    | DBS.length bytes == 1 && DBS.index bytes 0 == 0 = Just PointAtInfinity
    | DBS.length bytes == expLen && (DBS.index bytes 0 == 0x2 || DBS.index bytes 0 == 0x03) = result
        where
         expLen = 1 + DBS.length (F.toBytes a)
         x = (+ a*0) <$> F.fromBytes (DBS.drop 1 bytes)  -- a*0 forces type
         sgn0y = if DBS.index bytes 0 == 0x02 then 0 else 1::Integer
         alpha = (\t -> t^(3::Integer) + a * t + b) <$> x
         beta = alpha >>= F.sqrt
         y =  (\t -> if F.sgn0 t == sgn0y then t else negate t) <$> beta
         result = Affine <$> x <*> y
_fromBytes _ _ _ = Nothing


-- _isOnCurve :: (F.Field a) => Point a -> a -> a -> Bool
-- _isOnCurve (Projective x y z) a b = z*y^(2::Integer) == x^(3::Integer) + a*x*z^(2::Integer) + b*z^(3::Integer)
-- _isOnCurve (Affine x y) a b = y^(2::Integer) == x^(3::Integer) + a*x + b
-- _isOnCurve PointAtInfinity _ _ = True


_negatePt :: (F.Field a) => Point a -> Point a
_negatePt (Projective x y z) = Projective x (- y) z
_negatePt PointAtInfinity = PointAtInfinity
_negatePt a = _negatePt $ _toProjective a


-- See https://eprint.iacr.org/2015/1060.pdf page 8; The following has all the additions 'squashed out'
-- Algorithm 1: Complete, projective point addition for arbitrary prime order short Weierstrass curves E/Fq : y^2 = x^3 + ax + b.
_pointAdd :: (F.Field a) => Point a -> Point a -> a -> a -> Point a
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
_pointMul :: (F.Field a, F.Field b) => b -> Point a -> Point a -> a -> a -> Point a
_pointMul scalar pt accum a b3
  | scalar == 0 = accum
  | F.sgn0 scalar /= 0 = _pointMul (F.shiftR1 scalar) doublePt (_pointAdd accum pt a b3) a b3
  | F.sgn0 scalar == 0 = _pointMul (F.shiftR1 scalar) doublePt accum a b3
  | otherwise = panic "_pointMul pattern match fail (should never happen)"
  where
    doublePt = _pointAdd pt pt a b3


_toAffine :: (F.Field a) => Point a -> Point a
_toAffine (Projective _ _ 0) = PointAtInfinity
_toAffine (Projective x y z) = Affine (x * F.inv0 z) (y * F.inv0 z)
_toAffine (Affine x y) = Affine x y
_toAffine PointAtInfinity = PointAtInfinity


-- Compressed; section 2.3.3 on page 10 of https://www.secg.org/sec1-v2.pdf
_toBytes :: F.Field a => Point a -> ByteString
_toBytes PointAtInfinity = DBS.pack [0]
_toBytes (Projective x y z) = _toBytes (_toAffine (Projective x y z))
_toBytes (Affine x y)
  | F.sgn0 y == 0 = DBS.cons 0x02 (F.toBytes x)
  | otherwise     = DBS.cons 0x03 (F.toBytes x)


_toProjective :: F.Field a => Point a -> Point a
_toProjective (Projective x y z) = Projective x y z
_toProjective (Affine x y) = Projective x y 1
_toProjective PointAtInfinity = Projective 0 1 0
