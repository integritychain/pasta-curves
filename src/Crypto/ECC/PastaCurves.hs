{- |
Copyright: (c) 2022 Eric Schorn
SPDX-License-Identifier: MIT
Maintainer: Eric Schorn <eschorn@integritychain.com>

See README for more info
-}
{-# LANGUAGE CPP, DataKinds, KindSignatures, ImportQualifiedPost, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, Trustworthy #-}


module PastaCurves
    ( projectName, a, b, c, main11
    ) where

import Protolude

import Constants qualified as Co
import Fields qualified as F
import Curves qualified as C


main11 :: IO ()
main11 = do
  print ("hello, world" :: Text)
  print (F.sqrt a)
  print (F.sqrt b)
  print (C.neutral :: C.Pallas)


a :: $(F.primeField Co.pallasPrime)
a = 9


b :: $(F.primeField Co.vestaPrime)
b = 9


c :: $(F.primeField Co.pallasPrime)
c = 123*123


projectName :: Text
projectName = "pasta-curves"
