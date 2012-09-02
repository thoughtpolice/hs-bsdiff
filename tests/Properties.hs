module Main
       ( main -- :: IO ()
       ) where

import Test.QuickCheck
import Test.HUnit

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
