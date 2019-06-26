{-# language
        ScopedTypeVariables
  #-}

module Main (main) where

import Test.QuickCheck.Classes
import Test.QuickCheck (Arbitrary(..))

import Data.Complex (Complex(..))
import GHC.Fingerprint (Fingerprint(..))
import GHC.Real (Ratio(..))
import Data.Proxy (Proxy(..))

import Data.Primitive.Instances ()

main :: IO ()
main = lawsCheckMany
  [ ("Complex Double", [primLaws (Proxy :: Proxy (Complex Double))])
  , ("Ratio Double", [primLaws (Proxy :: Proxy (Ratio Int))])
  , ("Fingerprint", [primLaws (Proxy :: Proxy Fingerprint)])
  ]

instance Arbitrary Fingerprint where
  arbitrary = Fingerprint <$> arbitrary <*> arbitrary
