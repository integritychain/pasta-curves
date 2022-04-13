{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings, Trustworthy, ImportQualifiedPost, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestCurves (curveProps) where

import Protolude
import Test.Tasty qualified as TT
import Test.Tasty.QuickCheck qualified as TTQC

import Curves
import TestFields ()

instance TTQC.Arbitrary Pallas where
  arbitrary = do
    scalar <- TTQC.arbitrary
    return $ pointMul (scalar :: Fq) (base :: Pallas)  

instance TTQC.Arbitrary Vesta where
  arbitrary = do
    scalar <- TTQC.arbitrary
    return $ pointMul (scalar :: Fp) (base :: Vesta)  


curveProps :: TT.TestTree
curveProps = TT.testGroup "(checked by QuickCheck)" [
  TTQC.testProperty "pallas point add/mul" $ 
    \a b -> pointAdd (pointMul (a :: Fq) (base :: Pallas)) (pointMul b base) == pointMul (a+b) base,
  TTQC.testProperty "pallas point add symm" $
    \a b -> pointAdd a b == pointAdd b (a :: Pallas),

  TTQC.testProperty "vesta point add/mul" $ 
    \a b -> pointAdd (pointMul (a :: Fp) (base :: Vesta)) (pointMul b base) == pointMul (a+b) base,
  TTQC.testProperty "vesta point add symm" $
    \a b -> pointAdd a b == pointAdd b (a :: Vesta) 

  ]

