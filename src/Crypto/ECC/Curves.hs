{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleInstances, ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, Trustworthy #-}

module Curves (CurvePt(base, fromBytes, isOnCurve, negatePt, neutral, pointAdd, toAffine, toBytes, toProjective),
               Curve(pointMul), Fp, Fq, Pallas, Vesta) where

import Protolude
import Data.ByteString qualified as DBS
import Data.Maybe qualified as DM
import Fields qualified as F
import Constants qualified as C
import Prelude (error)

-- TODO : Deserialization; serialization
--  https://www.secg.org/sec1-v2.pdf
--  compressed points

-- ** SERIALIZATION
--  if P=POI, res = [0x00]
--  else
--    1. convert x co-ordinate to octect string (use point deser) => X
--    2. if y mod 2 == 0, then Y => 02
--    3. if y mod 2 == 1, then Y => 03
--  return Y || X


-- ** DESERIALIZATION
-- if M = [0x00] (length = 1 byte) then return Just neutral
-- check length (field element length + 1) and decode as Y (one byte) || X
-- decode X; return on error (MAYBE THIS SHOULD BE EITHER?)
-- if Y = [0x02] then y = 0, else Y = [0x03] then y = 1, else return as error
-- compute alpha (which corresponds to y^2) as x^3 + ax + b
-- compute beta as root of alpha, return on no root
-- if beta mod 2 = y, then return co-ord y=B, else return co-ord p - b


data Point a = Projective {_px :: a, _py :: a, _pz :: a} -- (x * inv0 z, y * inv0 z)
               | Affine {_ax :: a, _ay :: a}
               | PointAtInfinity deriving stock (Show)

-- is there POI in Affine (no; yes 0 0)?  INTERESTING; RESULTS SHOULD ALWAYS STAY PROJECTIVE THEN!!??
-- so only works for curves where b != 0

instance (F.Field a, Eq a) => Eq (Point a) where
  (==) (Affine x1 y1) (Affine x2 y2) = (x1 == x2) && (y1 == y2)
  (==) p1 p2 = _toAffine p1 == _toAffine p2

type Fp = $(F.primeField C.pallasPrime)
type Fq = $(F.primeField C.vestaPrime)
newtype Pallas = Pallas (Point Fp) deriving stock (Show, Eq)
newtype Vesta  = Vesta  (Point Fq) deriving stock (Show, Eq)


class CurvePt a where
  base :: a
  fromBytes :: ByteString -> Maybe a
  isOnCurve :: a -> Bool
  negatePt :: a -> a
  neutral :: a
  pointAdd :: a -> a -> a
  toAffine :: a -> a
  toBytes :: a -> ByteString
  toProjective :: a -> a


instance CurvePt Pallas where
  base = Pallas $ Projective 1 0x248b4a5cf5ed6c83ac20560f9c8711ab92e13d27d60fb1aa7f5db6c93512d546 1
  isOnCurve (Pallas p) = _isOnCurve p 0 5 -- (b3 / 3)
  negatePt (Pallas a) = Pallas $ _negatePt a
  neutral = Pallas $ Projective 0 1 0
  pointAdd (Pallas p1) (Pallas p2) = Pallas $ _pointAdd p1 p2 0 15
  toAffine (Pallas a) = Pallas $ _toAffine a
  toProjective (Pallas a) = Pallas $ _toProjective a

  fromBytes b
    | DBS.length b == 1 && DBS.index b 0 == 0 = Just neutral
    | DBS.length b == expectedB && (DBS.index b 0 == 0x2 || DBS.index b 0 == 0x03) = result
        where
         expectedB = 1 + DBS.length (F.toBytes (0 :: Fp))
         x = F.fromBytes (DBS.drop 1 b) :: Maybe Fp
         lead = if DBS.index b 0 == 0x02 then 0 else 1 :: Integer
         alpha = (\z -> z^(3 :: Int) + 0 * z + 5) <$> x :: Maybe Fp
         beta = if isNothing alpha then Nothing else F.sqrt $ DM.fromJust alpha :: Maybe Fp -- more elegant way?
         y_m =  (\z -> if F.sgn0 z == lead then z else negate z) <$> beta :: Maybe Fp  -- TODO: include 0x02/0x03, isOnCurve
         result = (\z y -> Pallas $ Affine z y) <$> x <*> y_m :: Maybe Pallas
  fromBytes _ = Nothing

  toBytes (Pallas PointAtInfinity) = DBS.pack [0]
  toBytes (Pallas (Projective x y z)) = toBytes (toAffine (Pallas (Projective x y z)))
  toBytes (Pallas (Affine x y))
       | F.sgn0 y == 0 = DBS.cons 0x02 (F.toBytes x)
       | F.sgn0 y == 1 = DBS.cons 0x03 (F.toBytes x)
       | otherwise = panic "toBytes error should never happen"

instance CurvePt Vesta where
  base = Vesta $ Projective  1 0x26bc999156dd5194ec49b1c551768ab375785e7ce00906d13e0361674fd8959f 1
  fromBytes _ = panic "fromBytes not yet implemented"
  isOnCurve _ = False
  negatePt (Vesta a) = Vesta $ _negatePt a
  neutral = Vesta $ Projective 0 1 0
  pointAdd (Vesta p1) (Vesta p2) = Vesta $ _pointAdd p1 p2 0 15 :: Vesta
  toAffine (Vesta a) = Vesta $ _toAffine a
  toBytes _ = panic "toBytes not yet implemented"
  toProjective (Vesta a) = Vesta $ _toProjective a


class (CurvePt a, F.Field b) => Curve a b where
  pointMul :: b -> a -> a


instance Curve Pallas Fq where
  pointMul aa (Pallas p1) = Pallas $ _pointMul (F.toI aa) p1 (Projective 0 1 0) 0 15


instance Curve Vesta Fp where
  pointMul aa (Vesta p1) = Vesta $ _pointMul (F.toI aa) p1 (Projective 0 1 0) 0 15


_toAffine :: (F.Field a) => Point a -> Point a
_toAffine (Projective x1 y1 z1) = Affine (x1 * F.inv0 z1) (y1 * F.inv0 z1)
_toAffine (Affine x y) = Affine x y
_toAffine _ = error "hmmmmmm"

_isOnCurve :: (F.Field a, Eq a) => Point a -> a -> a -> Bool
_isOnCurve (Projective x y z) a b = z*y^(2::Integer) == x^(3::Integer) + a*x*z^(2::Integer) + b*z^(3::Integer)
_isOnCurve p a b = _isOnCurve (_toProjective p) a b


_toProjective :: F.Field a => Point a -> Point a
_toProjective (Affine x y) = Projective x y 1
_toProjective _ = panic "Projective -> projective not implemented"


-- See https://eprint.iacr.org/2015/1060.pdf page 8; The following has all the additions 'squashed out'
-- Algorithm 1: Complete, projective point addition for arbitrary prime order short Weierstrass curves E/Fq : y^2 = x^3 + ax + b.
_pointAdd :: (F.Field a) => Point a -> Point a -> Integer -> Integer -> Point a
_pointAdd (Projective x1 y1 z1) (Projective x2 y2 z2) a b3 = result
  where
        m0 = x1 * x2
        m1 = y1 * y2
        m2 = z1 * z2
        m3 = (x1 + y1) * (x2 + y2)
        m4 = (x1 + z1) * (x2 + z2)
        m5 = (y1 + z1) * (y2 + z2)
        m6 = fromInteger a * (- m0 - m2 + m4)
        m7 = fromInteger b3 * m2
        m8 = (m1 - m6 - m7) * (m1 + m6 + m7)
        m9 = fromInteger a * m2
        m10 = fromInteger b3 * (- m0 - m2 + m4)
        m11 = fromInteger a * (m0 - m9)
        m12 = (m0 * 3 + m9) * (m10 + m11)
        m13 = (- m1 - m2 + m5) * (m10 + m11)
        m14 = (- m0 - m1 + m3) * (m1 - m6 - m7)
        m15 = (- m0 - m1 + m3) * (m0 * 3 + m9)
        m16 = (- m1 - m2 + m5) * (m1 + m6 + m7)
        result = Projective (-m13 + m14) (m8 + m12) (m15 + m16)
_pointAdd p1 p2 a b3 = _pointAdd (_toProjective p1) (_toProjective p2) a b3


_negatePt :: (F.Field a) => Point a -> Point a
_negatePt (Projective x y z) = Projective x (- y) z
_negatePt a = _negatePt $ _toProjective a


-- scalar * point, neutral, a, b3 -> point
_pointMul :: (F.Field a, Eq a) => Integer -> Point a -> Point a -> Integer -> Integer -> Point a
_pointMul scalar pt accum a b3
  | scalar < 0 = _pointMul (- scalar) (_negatePt pt) accum a b3
  | scalar == 0 = accum
  | odd scalar  = _pointMul (shiftR scalar 1) doublePt (_pointAdd accum pt a b3) a b3
  | even scalar = _pointMul (shiftR scalar 1) doublePt accum (fromInteger a) (fromInteger b3)
  | otherwise = panic "_pointMul pattern match fail (should never happen)"
  where
    doublePt = _pointAdd pt pt a b3
