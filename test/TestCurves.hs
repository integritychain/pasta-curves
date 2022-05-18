{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, Trustworthy, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestCurves (curveProps, testPOI) where

import Prelude
import Data.ByteString (pack)
import Data.Maybe (fromJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), testProperty)
import Test.Tasty.HUnit (assertBool, testCase)


import PastaCurves
import TestFields ()

instance Arbitrary Pallas where
  arbitrary = do
    scalar <- arbitrary
    return $ pointMul (scalar :: Fq) (base :: Pallas)  

instance Arbitrary Vesta where
  arbitrary = do
    scalar <- arbitrary
    return $ pointMul (scalar :: Fp) (base :: Vesta)  


curveProps :: TestTree
curveProps = testGroup "Testing Curve properties via QuickCheck" [
  testProperty "Pallas point add/mul" $ 
    \x y -> pointAdd (pointMul (x :: Fq) base) (pointMul y base) == pointMul (x+y) (base :: Pallas),
  testProperty "Pallas point add symm" $
    \x y -> pointAdd x y == pointAdd y (x :: Pallas),
  testProperty "Pallas ser->deser" $
    \x -> fromJust (fromBytes (toBytes x)) == (x :: Pallas),


  testProperty "Vesta point add/mul" $ 
    \x y -> pointAdd (pointMul (x :: Fp) base) (pointMul y base) == pointMul (x+y) (base :: Vesta),
  testProperty "Vesta point add symm" $
    \x y -> pointAdd x y == pointAdd y (x :: Vesta),
  testProperty "Vesta ser->deser" $
    \x -> fromJust (fromBytes (toBytes x)) == (x :: Vesta)
 
-- TODO: deser POI, 'bad curve pt' where y has no square root, negatePt, add a neutral

  ]
-- --------------? HMMMM
testPOI :: TestTree
testPOI = testCase "huTstPairingMul" $
  do
    let poi_bytes = pack [0]
--    let poi1 = fromBytes  poi_bytes :: Maybe Pallas
    let om1 = (0::Fq) - 1
    let poi2 = pointAdd (pointMul om1 base) base :: Pallas
--    let poi3 = neutral :: Pallas
    let poi4 = toBytes  poi2
    assertBool "bad pairing mul" (poi_bytes == poi4) --       ((==) <$> leftSide <*> rightSide) == Just True)
