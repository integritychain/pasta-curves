{-# LANGUAGE DataKinds, FlexibleInstances, NoImplicitPrelude, OverloadedStrings, Trustworthy, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module TestFields (fieldProps, testH2Fp) where

import Prelude hiding (sqrt)
import Control.Monad (replicateM)
import Data.ByteString (ByteString, pack)
import Data.Maybe (fromJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (Arbitrary(..), choose, testProperty)

import PastaCurves
import Constants


instance Arbitrary Fp where
   arbitrary = do
     fromInteger <$> choose (0, pallasPrime - 1)


instance Arbitrary Fq where
   arbitrary = do
     fromInteger <$> choose (0, vestaPrime - 1)

type Serdes = ByteString

instance Arbitrary Serdes where
  arbitrary = pack <$> replicateM 32 arbitrary


fieldProps :: TestTree
fieldProps = testGroup "Testing Field properties via QuickCheck" [
  testProperty "Fp arith"  $ \a b c -> a*(b-c) - a*b + a*c == (0 :: Fp),
  testProperty "Fp inv0"   $ \a -> a * inv0 a == (1 :: Fp),
  testProperty "Fp sqrt"   $ \a -> fromJust (sqrt (a*a))^(2 :: Integer) == (a^(2 :: Integer) :: Fp),
  testProperty "Fp serdes" $ \a -> fromJust (fromBytesF  (toBytesF (_fromBytesF a ::
       Fp))) == (_fromBytesF a :: Fp),

  testProperty "Fq arith"  $ \a b c -> a*(b-c) - a*b + a*c == (0 :: Fq),
  testProperty "Fq inv0"   $ \a -> a * inv0 a == (1 :: Fq),
  testProperty "Fq sqrt"   $ \a -> fromJust (sqrt (a*a))^(2 :: Integer) == (a^(2 :: Integer)  :: Fq),
  testProperty "Fq serdes" $ \a -> fromJust (fromBytesF  (toBytesF (_fromBytesF a ::
       Fq))) == (_fromBytesF a :: Fq)
  ]

testH2Fp :: TestTree
testH2Fp = testCase "testH2Fp" $ assertBool "Failed H2Fp" helper
  where
    (r1, r2) = hash2Field (pack [4,5,6]) "\x01\x02" "pallas" :: (Fp, Fp)
    e1 = 0x2479644355b8886ebd8b5d6e15ef5e0918e67ab56f830ef65fcfa9826c66ad35 :: Fp
    e2 = 0x1977de721cac345d07f9f9b954deab2ce4ed43fab979d78f695980cd188fbace :: Fp
    helper = (r1 == e1) && (r2 == e2)
