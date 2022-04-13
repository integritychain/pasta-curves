{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings, TemplateHaskell, Trustworthy, ImportQualifiedPost, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestFields (fieldProps, testH2Fp) where

import Protolude

import Data.ByteString qualified as DBS
import Data.Maybe qualified as DM
import Test.Tasty qualified as TT
import Test.Tasty.HUnit qualified as TTHU
import Test.Tasty.QuickCheck qualified as TTQC

import Fields qualified as F
import Constants qualified as C


type Fp = $(F.primeField C.pallasPrime)
type Fq = $(F.primeField C.vestaPrime)

instance TTQC.Arbitrary Fp where  --  $(F.primeField C.pallasPrime) where
   arbitrary = do
     fromInteger <$> TTQC.choose (0, C.pallasPrime - 1)


instance TTQC.Arbitrary Fq where  --   $(F.primeField C.vestaPrime) where
   arbitrary = do
     fromInteger <$> TTQC.choose (0, C.vestaPrime - 1)

type Serdes = DBS.ByteString

instance TTQC.Arbitrary Serdes where
  arbitrary = DBS.pack <$> replicateM 32 TTQC.arbitrary


fieldProps :: TT.TestTree
fieldProps = TT.testGroup "(checked by QuickCheck)" [
  TTQC.testProperty "pallas fe arith"  $ \a b c -> a*(b-c) - a*b + a*c == (0 :: Fp),
  TTQC.testProperty "pallas fe inv0"   $ \a -> a * F.inv0 a == (1 :: Fp),
  TTQC.testProperty "pallas fe sqrt"   $ \a -> DM.fromJust (F.sqrt (a*a))^(2 :: Integer) == (a^(2 :: Integer) :: Fp),
  TTQC.testProperty "pallas fe serdes" $ \a -> DM.fromJust (F.fromBytes  (F.toBytes (F._fromBytes a ::
       $(F.primeField C.pallasPrime)))) == (F._fromBytes a :: Fp),

  TTQC.testProperty "vesta fe arith"   $ \a b c -> a*(b-c) - a*b + a*c == (0 :: Fq),
  TTQC.testProperty "vesta fe inv0"    $ \a -> a * F.inv0 a == (1 :: Fq),
  TTQC.testProperty "vesta fe sqrt"    $ \a -> DM.fromJust (F.sqrt (a*a))^(2 :: Integer) == (a^(2 :: Integer)  :: Fq),
  TTQC.testProperty "vesta fe serdes"  $ \a -> DM.fromJust (F.fromBytes  (F.toBytes (F._fromBytes a ::
       $(F.primeField C.vestaPrime)))) == (F._fromBytes a :: Fq)
  ]

testH2Fp :: TT.TestTree
testH2Fp = TTHU.testCase "testH2Fp" $ TTHU.assertBool "Failed H2Fp" helper
  where
    (r1, r2) = F.hash2Field (DBS.pack [4,5,6]) "\x01\x02" "pallas" :: (Fp, Fp)
    e1 = 0x2479644355b8886ebd8b5d6e15ef5e0918e67ab56f830ef65fcfa9826c66ad35 :: Fp
    e2 = 0x1977de721cac345d07f9f9b954deab2ce4ed43fab979d78f695980cd188fbace :: Fp
    helper = (r1 == e1) && (r2 == e2)
