{-# OPTIONS_GHC -fno-cse #-}
module Main
       ( main -- :: IO ()
       ) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()

import qualified Data.ByteString as S(null)
import Data.ByteString.BSDiff

main :: IO ()
main = hspec $ do
  -------------------------------------------------------------------------------
  -- Diffing
  describe "diffing" $
    it "is a pure operation" $ property $
     \old new -> diff old new == diff old new

  -------------------------------------------------------------------------------
  -- Patching
  describe "patching" $ do
    it "is a pure operation" $ property $
      \old new -> (patch old =<< diff old new) == (patch old =<< diff old new)

    it "returns a new version, given a patch and an old version" $ property $
      \old new -> not (S.null old || S.null new) ==>
                   Just new == (patch old =<< diff old new)

