{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances where

import Miso.IndexDb (DbName(..), ms)
import Test.QuickCheck (Arbitrary(..), Gen, listOf1)

instance Arbitrary DbName where
  arbitrary = DbName . ms <$> (listOf1 arbitrary :: Gen String)
