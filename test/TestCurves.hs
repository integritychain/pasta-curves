{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings, Trustworthy, ImportQualifiedPost, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestCurves (curveProps, testPOI) where

import Protolude
import Data.ByteString qualified as DBS
import Test.Tasty qualified as TT
import Test.Tasty.QuickCheck qualified as TTQC
import Test.Tasty.HUnit qualified as TTHU


import Curves
import TestFields ()
import Data.Maybe (fromJust)
-- import qualified GHC.Generics as DM
-- import qualified Data.Maybe as DM

instance TTQC.Arbitrary Pallas where
  arbitrary = do
    scalar <- TTQC.arbitrary
    return $ pointMul (scalar :: Fq) (base :: Pallas)  

instance TTQC.Arbitrary Vesta where
  arbitrary = do
    scalar <- TTQC.arbitrary
    return $ pointMul (scalar :: Fp) (base :: Vesta)  


curveProps :: TT.TestTree
curveProps = TT.testGroup "Testing Curve properties via QuickCheck" [
  TTQC.testProperty "Pallas point add/mul" $ 
    \a b -> pointAdd (pointMul (a :: Fq) base) (pointMul b base) == pointMul (a+b) (base :: Pallas),
  TTQC.testProperty "Pallas point add symm" $
    \a b -> pointAdd a b == pointAdd b (a :: Pallas),
  TTQC.testProperty "Pallas ser->deser" $
    \a -> fromJust (fromBytes (toBytes a)) == (a :: Pallas),


  TTQC.testProperty "Vesta point add/mul" $ 
    \a b -> pointAdd (pointMul (a :: Fp) base) (pointMul b base) == pointMul (a+b) (base :: Vesta),
  TTQC.testProperty "Vesta point add symm" $
    \a b -> pointAdd a b == pointAdd b (a :: Vesta),
  TTQC.testProperty "Vesta ser->deser" $
    \a -> fromJust (fromBytes (toBytes a)) == (a :: Vesta)
 
-- TODO: deser POI, 'bad curve pt' where y has no square root, negatePt, add a neutral

  ]
-- --------------? HMMMM
testPOI :: TT.TestTree
testPOI = TTHU.testCase "huTstPairingMul" $
  do
    let poi_bytes = DBS.pack [0]
--    let poi1 = fromBytes  poi_bytes :: Maybe Pallas
    let om1 = (0::Fq) - 1
    let poi2 = pointAdd (pointMul om1 base) base :: Pallas
--    let poi3 = neutral :: Pallas
    let poi4 = toBytes  poi2
    TTHU.assertBool "bad pairing mul" (poi_bytes == poi4) --       ((==) <$> leftSide <*> rightSide) == Just True)
