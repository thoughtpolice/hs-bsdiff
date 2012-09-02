module Main
       ( main -- :: IO ()
       ) where

import Test.Hspec
import Test.QuickCheck

import Data.ByteString
import Data.ByteString.BSDiff

main :: IO ()
main = hspec $ do
  -------------------------------------------------------------------------------
  -- Diffing
  describe "diffing" $ return ()

  -------------------------------------------------------------------------------
  -- Patching
  describe "patching" $ return ()
