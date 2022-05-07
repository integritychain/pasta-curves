{-|
Module      : Crypto.PastaCurves.Fields
Description : Supports the instantiation of parameterized prime fields.
Copyright   : (c) Eric Schorn, 2022
License     : MIT
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental
Portability : GHC

This module provides a Template Haskell splice capable of generating a prime field of 
arbitrary modulus along with a variety of supporting functionality such as multiplicative
inverse, square testing, square root, serialization and deserialization, and hash2Field.
The algorithms are not constant time.
-}

{-# LANGUAGE CPP, DataKinds, DerivingStrategies, KindSignatures, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, Trustworthy #-}

module Fields (Field(_fromBytes, fromBytes, hash2Field, inv0, isSqr, sgn0, shiftR1, sqrt,
  toBytes, toI), primeField) where

import Prelude (Bool (..), Eq (..), Int, Integer, Integral (..), Maybe (..), Num (..), 
  Ord (..), Show (..), String, (.), (<$>), (++), (||), (^), ($), (!!), div, error, even, 
  fromIntegral, head, length, not, otherwise, reverse, until)
import Crypto.Hash (Blake2b_512 (Blake2b_512), hashWith)
import Data.Bifunctor (bimap)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.ByteArray (convert, length, xor)
import Data.ByteString (concat, foldl', pack, replicate)
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Char (chr)
import Data.Typeable (Proxy (Proxy))
import GHC.Word (Word8)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Language.Haskell.TH (TypeQ, litT, numTyLit)


-- | The `Fp p` (template) type holds a field element with a parameterized modulus of 
-- `p`; Note that the constructor is not itself exported.
newtype Fp (p::Nat) = Fp Integer deriving stock (Eq)


-- A CPP macro 'helper' to extract the modulus from (Fp p)
#define MOD natVal (Proxy :: Proxy p)


-- | The `primeField` function returns a Template Haskell splice that creates a concrete
-- `Fp` `p` type with the specified prime modulus `p`; Note that the primality of `p` is 
-- not checked.
primeField :: Integer -> TypeQ
primeField p
  | p < 3    = error "primeField: n must be larger than 2"
  | otherwise = [t| Fp $(litT (numTyLit p)) |]


-- | The `Fp p` type is an instance of the `Num` class.
instance KnownNat p => Num (Fp p) where
  fromInteger a = Fp $ a `mod` MOD
  (+) (Fp a) (Fp b) = fromInteger (a + b)
  (-) (Fp a) (Fp b) = fromInteger (a - b)
  (*) (Fp a) (Fp b) = fromInteger (a * b)
  abs = error "abs: not implemented"
  signum = error "signum: not implemented"


-- | The `Fp p` type is an instance of the `Show` class written in hexadecimal.
instance KnownNat p => Show (Fp p) where
  show (Fp a) = "0x" ++ ["0123456789ABCDEF" !! nibble n | n <- [e, e-1..0]]
    where
      nibble :: Int -> Int
      nibble n = fromInteger $ shiftR a (n*4) `mod` 16
      e = ((3 + until ((MOD <) . (2^)) (+1) 0) `div` 4) - 1 :: Int


-- | The `Field` class provides useful support functionality for field elements.
class (Num a, Eq a) => Field a where

  -- | The `fromBytes` function is the primary deserialization constructor which 
  -- consumes a big-endian `ByteString` sized to minimally contain the modulus 
  -- and returns a field element. The deserialized integer must already be properly 
  -- reduced to reside within [0..modulus).
  fromBytes :: ByteString  -> Maybe a

  -- | The `_fromBytes` function is the secondary deserialization constructor which
  -- consumes an unconstrained big-endian `ByteString` and returns a internally 
  -- reduced field element. This function is useful for random testing and 
  -- hash2Field-style funtions.
  _fromBytes :: ByteString -> a

  -- | The `hash2Field` function provides intermediate functionality that is suitable
  -- for ultimately supporting the `Curves.hash2Curve` function. This function 
  -- returns a 2-tuple of field elements.
  hash2Field :: ByteString -> String -> String -> (a, a)

  -- | The `inv0` function returns the multiplicative inverse as calculated by 
  -- Fermat's Little Theorem (which maps 0 to 0).
  inv0 :: a -> a

  -- | The `isSqr` function indicates whether the operand has a square root.
  isSqr :: a -> Bool

  -- | The `sgn0` function effectively returns the least significant bit of the field
  -- element as an Integer.
  sgn0 :: a -> Integer

  -- | The `shiftR1` function effectively divides the field element by two.
  shiftR1 :: a -> a

  -- | The `Fields.sqrt` function implements the variable-time Tonelli-Shanks 
  -- algorithm to calculate the operand's square root. The function returns `Nothing`
  -- in the event of a problem (such as the operand not being a square, the modulus 
  -- is not prime, etc).
  sqrt :: a -> Maybe a

  -- | The `toBytes` function serializes an element into a big-endian `ByteString` 
  -- sized to minimal contain the modulus.
  toBytes :: a -> ByteString

  -- | The `toI` function returns the field element as a properly reduced Integer.
  toI :: a -> Integer


-- | The `Fp p` type is an instance of the `Field` class. These functions are largely 
-- simple adapters to the more generic internal functions implemented further below.
instance KnownNat p => Field (Fp p) where

  -- Validated deserialization, returns a Maybe field element.
  -- fromBytes :: ByteString  -> Maybe a
  fromBytes bytes | Data.ByteArray.length bytes /= expLen || integer >= MOD = Nothing
                  | otherwise = Just $ fromInteger integer
    where
      expLen = (7 + until ((MOD <) . (2^)) (+1) 0) `div` 8 :: Int
      integer = foldl' (\a b -> a `shiftL` 8 .|. fromIntegral b) 0 bytes :: Integer


  -- Unvalidated deserialization, returns reduced field element.
  -- _fromBytes :: ByteString -> a
  _fromBytes bytes = fromInteger $ foldl' (\a b -> a `shiftL` 8 .|. fromIntegral b) 0 bytes


  -- Supports for the hash2Curve function, returns pair of field elements.
  -- hash2Field :: ByteString -> Text -> Text -> (a, a)
  hash2Field msg domPref curveId = bimap _fromBytes _fromBytes $ _h2f msg domPref curveId


  -- Multiplicative inverse, with 0 mapped to 0.
  -- inv0 :: a -> a
  inv0 (Fp a) = fromInteger $ _powMod a (MOD - 2) (MOD)


  -- Determines if the operand has a square root.
  -- isSqr :: a -> Bool
  isSqr (Fp a) = _isSqr a (MOD)


  -- Returns the least significant bit of the field element
  -- sgn0 :: a -> Integer
  sgn0 (Fp a) = a `mod` 2


  -- Diveds the element by 2
  -- shiftR1 :: a -> a
  shiftR1 (Fp a) = Fp (a `div` 2)


  -- Returns square root as Maybe field element. If problems, returns Nothing.
  -- sqrt :: a -> Maybe a
  sqrt (Fp a) = fromInteger <$> _sqrtVt a (MOD) s p c
    where  -- rewrite (modulus - 1) as p * 2**s 
      s = until ((/= 0) . ((MOD -1) `rem`) . (2^)) (+1) 0 - 1 :: Integer
      p = (MOD - 1) `div` (2^s)
      z = head ([x | x <- [1..], not (_isSqr x (MOD))] ++ [0]) -- 1st non-square
      c = _powMod z p (MOD)  -- 'fountain of fixes'


  -- Deserialization.
  -- toBytes :: a -> ByteString
  toBytes (Fp a) = pack $ reverse res
    where
      expLen = (7 + until ((MOD <) . (2^)) (+1) 0) `div` 8 :: Integer
      res = [fromIntegral (shiftR a (8*b)) | b <- [0..(fromIntegral expLen - 1)]] :: [Word8]


  -- Returns the element as an Integer
  -- toI :: a -> Integer 
  toI (Fp a) = a


-- Complex/common support functions operating on integers rather than field elements

-- | Modular exponentiation.
-- _powMod :: operand -> exponent -> modulus
_powMod :: Integer -> Integer -> Integer -> Integer
_powMod _ e q | e < 0 || q < 3 = error "Invalid exponent/modulus"
_powMod _ 0 _ = 1
_powMod a 1 _ = a
_powMod a e q | even e = _powMod (a * a `mod` q) (shiftR e 1) q
              | otherwise = a * _powMod a (e - 1) q `mod` q


-- | Is operand a square vie Legendre symbol.
-- isSqr :: operand -> modulus
_isSqr :: Integer -> Integer -> Bool
_isSqr a q = (legendreSymbol == 0) || (legendreSymbol == 1)
  where legendreSymbol = _powMod a ((q - 1) `div` 2) q


-- | Variable-time Tonelli-Shanks algorithm
-- _sqrtVt :: operand -> modulus -> 's' -> 'p' -> nonSquare
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
            s_n = head ([i | i <- [1..(ss - 1)], _powMod tt (2^i) q == 1] ++ [0]) :: Integer
            ff = _powMod cc (2^(ss - s_n - 1)) q
            r_n = rr * ff `mod` q
            t_n = (tt * _powMod ff 2 q) `mod` q
            c_n = _powMod cc (2^(ss - s_n)) q


-- hash2field per Zcash Pasta Curve construction (similar but not idential 
-- to the CFRG hash-to-curve specification)
-- Fortuitously, hash personalization is set to all zeros https://github.com/haskell-crypto/cryptonite/issues/333
-- _h2f :: message -> domain prefix -> curve ID
_h2f :: ByteString -> String -> String ->  (ByteString, ByteString)
_h2f msg domPrefix curveId = (digest1, digest2)
  where
    prefix = Data.ByteString.replicate 128 0
    suffix = fromString (domPrefix ++ "-" ++ curveId ++ "_XMD:BLAKE2b_SSWU_RO_" ++
             [chr (22 + Prelude.length curveId + Prelude.length domPrefix)])
    mkDigest :: ByteString -> ByteString
    mkDigest x = convert $ hashWith Blake2b_512 x
    digest0 = mkDigest $ Data.ByteString.concat [prefix, msg, pack [0,0x80,0], suffix]
    digest1 = mkDigest $ Data.ByteString.concat [digest0, pack [0x01], suffix]
    mix = xor digest0 digest1 :: ByteString
    digest2 = mkDigest $ Data.ByteString.concat [mix, pack [0x02], suffix]
