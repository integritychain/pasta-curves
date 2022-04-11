{-|
Module      : Crypto.PastaCurves.Fields
Description : Supports the instantiation of parameterized prime fields.
Copyright   : (c) Eric Schorn, 2022
License     : MIT
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental
Portability : GHC

This module provides a Template Haskell splice capable of generating
a prime field of arbitrary modulus along with a variety of supporting
functionality such as multiplicative inverse, square testing, square root, 
serialization and deserialization, and hash2Field. The algorithms are not
constant time.
-}

{-# LANGUAGE AllowAmbiguousTypes, DataKinds, KindSignatures  #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, CPP, Trustworthy, ImportQualifiedPost, DerivingStrategies #-}
{-# OPTIONS_GHC -Weverything -Wno-missing-import-lists -Wno-unsafe -Wno-all-missed-specialisations #-}

module Fields (Field(_fromBytes, fromBytes, hash2Field, inv0, isSqr, sqrt, 
               toBytes, toI), primeField) where

import Protolude
import Protolude.Base (show)
import Crypto.Hash qualified as CH 
import Data.ByteArray qualified as DBA
import Data.ByteString qualified as DBS
import Data.Text qualified as DT
import Language.Haskell.TH qualified as LHTH


-- | The `Fp p` (template) type holds a field element with a parameterized
--   modulus of `p`; Note that the constructor is not itself exported.
newtype Fp (p::Nat) = Fp Integer deriving stock (Eq)


-- A CPP macro 'helper' to extract the modulus from (Fp p)
#define MOD natVal (Proxy :: Proxy p)


-- | The `primeField` function returns a Template Haskell splice that
--   creates a concrete `Fp` `p` type with the specified prime modulus 
--   `p`; Note that the primality of `p` is not checked.
primeField :: Integer -> LHTH.TypeQ
primeField p
  | p < 3    = panic "primeField: n must be larger than 2"
  | otherwise = [t| Fp $(LHTH.litT (LHTH.numTyLit p)) |]


-- | The `Fp p` type is an instance of the `Num` class.
instance KnownNat p => Num (Fp p) where
  fromInteger a = Fp $ a `mod` MOD
  (+) (Fp a) (Fp b) = fromInteger (a + b)
  (-) (Fp a) (Fp b) = fromInteger (a - b)
  (*) (Fp a) (Fp b) = fromInteger (a * b)
  abs = panic "abs: not implemented"
  signum = panic "signum: not implemented"


-- | The `Fp p` type is an instance of the `Show` class in hexadecimal.
instance KnownNat p => Show (Fp p) where
  show (Fp a) = "0x" ++ [DT.index "0123456789ABCDEF" $ nibble n | n <- [e, e-1..0]]
    where 
      nibble :: Int -> Int
      nibble n = fromInteger $ shiftR a (n*4) `mod` 16
      e = ((3 + until ((MOD <) . (2^)) (+1) 0) `div` 4) - 1 :: Int


-- | The `Field` class provides useful support functionality for field 
--   elements.
class Num a => Field a where

  -- | The `fromBytes` function is the primary deserialization constructor
  --   which consumes a big-endian `ByteString` sized to minimally contain
  --   the modulus and returns a field element. The deserialized integer
  --   must already be properly reduced to reside within [0..modulus).
  fromBytes :: ByteString  -> Maybe a

  -- | The `_fromBytes` function is the secondary deserialization 
  --   constructor which consumes an unconstrained big-endian `ByteString`
  --   and returns a internally reduced field element. This function is
  --   useful for random testing and hash2Field-style funtions.
  _fromBytes :: ByteString -> a

  -- | The `hash2Field` function provides intermediate functionality that 
  --   is suitable for ultimately supporting the `Curves.hash2Curve`
  --   function. This function returns a 2-tuple of field elements.
  hash2Field :: ByteString -> Text -> Text -> (a, a)

  -- | The `inv0` function returns the multiplicative inverse as 
  --   calculated by Fermat's Little Theorem (which maps 0 to 0).
  inv0 :: a -> a

  -- | The `isSqr` function indicates whether the operand has a square 
  --   root.
  isSqr :: a -> Bool

  -- | The `Fields.sqrt` function implements the variable-time 
  --   Tonelli-Shanks algorithm to calculate the operand's square root. 
  --   The function returns `Nothing` in the event of a problem (such
  --   as the operand not being a square, the modulus is not prime, etc).
  sqrt :: a -> Maybe a
 
  -- | The `toBytes` function serializes an element into a big-endian
  --   `ByteString` sized to minimal contain the modulus.
  toBytes :: a -> ByteString

  toI :: a -> Integer 


-- | The `Fp p` type is an instance of the `Field` class. These functions
--   are simple(r) 'adapters' to the more generic internal functions 
--   implemented further below.
instance KnownNat p => Field (Fp p) where

  -- Validated deserialization, returns a Maybe field element.
  -- fromBytes :: ByteString  -> Maybe a
  fromBytes bytes | DBS.length bytes /= expectedB || integer >= MOD = Nothing
                  | otherwise = Just $ fromInteger integer
    where
      expectedB = (7 + until ((MOD <) . (2^)) (+1) 0) `div` 8 :: Int
      integer = DBS.foldl' (\a b -> a `shiftL` 8 .|. fromIntegral b) 0 bytes :: Integer


  -- Unvalidated deserialization, returns reduced field element.
  -- _fromBytes :: ByteString -> a
  _fromBytes bytes = fromInteger $ DBS.foldl' (\a b -> a `shiftL` 8 .|. fromIntegral b) 0 bytes


  -- Supports for the hash2Curve function, returns pair of field elements.
  -- hash2Field :: ByteString -> Text -> Text -> (a, a)
  hash2Field msg domPref curveId = bimap _fromBytes _fromBytes $ _h2f msg domPref curveId


  -- Multiplicative inverse, with 0 mapped to 0.
  -- inv0 :: a -> a
  inv0 (Fp a) = fromInteger $ _powMod a (MOD - 2) (MOD)


  -- Determines if the operand has a square root.
  -- isSqr :: a -> Bool
  isSqr (Fp a) = _isSqr a (MOD)


  -- Returns square root as Maybe field element. If problems, returns Nothing.
  -- sqrt :: a -> Maybe a
  sqrt (Fp a) = fromInteger <$> _sqrtVt a (MOD) s p c
    where  -- rewrite:  (modulus - 1) = p * 2**s 
      s = until ((/= 0) . ((MOD -1) `rem`) . (2^)) (+1) 0 - 1 :: Integer
      p = (MOD - 1) `div` (2^s)
      z = headDef 0 [x | x <- [1..], not (_isSqr x (MOD))] -- 1st non-square
      c = _powMod z p (MOD)  -- 'fountain of fixes'


  -- Deserialization.
  -- toBytes :: a -> ByteString
  toBytes (Fp a) = DBS.pack $ reverse res
    where
      requiredB = (7 + until ((MOD <) . (2^)) (+1) 0) `div` 8 :: Integer
      res = [fromIntegral (shiftR a (8*b)) | b <- [0..(fromIntegral requiredB - 1)]] :: [Word8]

  -- toInteger :: a -> Integer 
  toI (Fp a) = a

-- Complex/common support functions operating on integers rather than field elements

-- | Modular exponentiation.
_powMod :: Integer -> Integer -> Integer -> Integer
_powMod _ e q | e < 0 || q < 1 = panic "Invalid exponent/modulus"
_powMod _ 0 _ = 1
_powMod a 1 _ = a
_powMod a e q | even e = _powMod (a * a `mod` q) (shiftR e 1) q
              | otherwise = a * _powMod a (e - 1) q `mod` q


-- | Is operand a square vie Legendre symbol.
_isSqr :: Integer -> Integer -> Bool
_isSqr a q = (legendreSymbol == 0) || (legendreSymbol == 1)
  where legendreSymbol = _powMod a ((q - 1) `div` 2) q


-- | Variable-time Tonelli-Shanks algorithm
_sqrtVt :: Integer -> Integer -> Integer -> Integer -> Integer -> Maybe Integer
_sqrtVt 0 _ _ _ _ = Just 0
_sqrtVt a q _ _ _ | not (_isSqr a q) = Nothing
_sqrtVt _ _ _ _ 0 = Nothing
_sqrtVt a q s p c = Just result
  where
    t = _powMod a p q
    r = _powMod a ((p + 1) `div` 2) q
    result = loopy t r c s
      where
        loopy :: Integer -> Integer -> Integer -> Integer -> Integer
        loopy tt  _  _ ss | tt == 0 || ss == 0 = 0
        loopy  1 rr  _  _ = rr
        loopy tt rr cc ss = loopy t_n r_n c_n s_n
          where
            s_n = headDef 0 [i | i <- [1..(ss - 1)], _powMod tt (2^i) q == 1]
            ff = _powMod cc (2^(ss - s_n - 1)) q
            r_n = rr * ff `mod` q
            t_n = (tt * _powMod ff 2 q) `mod` q
            c_n = _powMod cc (2^(ss - s_n)) q


-- hash2field per Zcash Pasta Curve construction (similar but not idential 
-- to the CFRG hash-to-curve specification)
-- Fortuitously, hash personalization is set to all zeros https://github.com/haskell-crypto/cryptonite/issues/333
_h2f :: ByteString -> Text -> Text ->  (ByteString, ByteString)
_h2f msg domPrefix curveId = (digest1, digest2)
  where
    prefix = DBS.replicate 128 0
    suffix = DBS.concat [encodeUtf8 domPrefix, "-", encodeUtf8 curveId, "_XMD:BLAKE2b_SSWU_RO_",
             DBS.pack [22 + fromIntegral (DT.length curveId) + fromIntegral (DT.length domPrefix)]]
    mkDigest :: ByteString -> ByteString 
    mkDigest x = DBA.convert $ CH.hashWith CH.Blake2b_512 x
    digest0 = mkDigest $ DBS.concat [prefix, msg, DBS.pack [0,0x80,0], suffix]
    digest1 = mkDigest $ DBS.concat [digest0, DBS.pack [0x01], suffix]
    mix = DBA.xor digest0 digest1 :: ByteString
    digest2 = mkDigest $ DBS.concat [mix, DBS.pack [0x02], suffix]
