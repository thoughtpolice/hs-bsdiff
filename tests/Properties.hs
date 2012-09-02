module Main
       ( main -- :: IO ()
       ) where

import Test.QuickCheck
import Test.HUnit

import Data.ByteString.BSDiff
import qualified Text.Search.Whistlepig.IO as IO

main :: IO ()
main = hspec $ do
  -------------------------------------------------------------------------------
  -- Diffing
  describe "diffing" $ return ()

  -------------------------------------------------------------------------------
  -- Patching
  describe "patching" $ return ()
