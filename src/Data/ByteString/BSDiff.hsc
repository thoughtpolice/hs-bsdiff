{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- |
-- Module      : Data.ByteString.BSDiff
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : GHC probably
--
-- This module exports a simple 'ByteString' based interface to
-- bsdiff, which allows you to create and apply binary patches to
-- arbitrary files (for example, if you want to do incremental software
-- updates to users.)
--
module Data.ByteString.BSDiff
       ( -- * ByteString interface
         diff   -- :: ByteString -> ByteString -> Maybe ByteString
       , patch  -- :: ByteString -> ByteString -> Maybe ByteString

         -- * File interface
       , bsdiff   -- :: FilePath -> FilePath -> FilePath -> IO ()
       , bspatch  -- :: FilePath -> FilePath -> FilePath -> IO ()
       ) where

import System.FilePath()

import Data.Int
import Data.Word
import Control.Monad (void, liftM)

import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Unsafe as U

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <bsdiff.h>
#include <bspatch.h>

-- | Creates a delta between two 'ByteString's. Can be applied
-- to the old version using 'patch', resuling in the new version.
diff :: ByteString       -- ^ Old version
     -> ByteString       -- ^ New version
     -> Maybe ByteString -- ^ Resulting delta
diff _old _new
  | S.null _old = Nothing
  | S.null _new = Nothing
  | otherwise   = unsafePerformIO $
      U.unsafeUseAsCStringLen _old $ \(oldp, olds) ->
      U.unsafeUseAsCStringLen _new $ \(newp, news) -> do
        r <- SI.createAndTrim (fi news) $ \out -> do
          res <- c_bsdiff oldp (fi olds) newp (fi news) out
          return $ if (res <= 0) then 0 else res
        if (S.null r) then return Nothing else return (Just r)
{-# INLINEABLE diff #-}

-- | Apply a binary patch to input, resuling in new version of input.
patch :: ByteString       -- ^ Old version
      -> ByteString       -- ^ Delta produced by 'diff'
      -> Maybe ByteString -- ^ New version
patch _old _patch
  | S.null _old = Nothing
  | S.null _patch = Nothing
  | otherwise = unsafePerformIO $
      U.unsafeUseAsCStringLen _old $ \(oldp, olds) ->
      U.unsafeUseAsCStringLen _patch $ \(patchp, patchs) -> do
        let sz = c_bspatch_newsize patchp
        if (sz <= 0) then return Nothing
         else do
           Just `liftM` SI.create (fi sz)
             (void . c_bspatch oldp (fi olds) patchp (fi patchs))
{-# INLINEABLE patch #-}

-- | Create a patch file, based on an old version of a file
-- and a new version. Ignores errors.
bsdiff :: FilePath -- ^ Old file
       -> FilePath -- ^ New file
       -> FilePath -- ^ Patch file to create
       -> IO ()
bsdiff _oldf _newf _patchf = do
  o <- S.readFile _oldf
  n <- S.readFile _newf
  maybe (return ()) (S.writeFile _patchf) (diff o n)

-- | Apply a patch file to an old version of a file, resulting
-- in a new version. Ignores errors.
bspatch :: FilePath -- ^ Old file
        -> FilePath -- ^ Patch file
        -> FilePath -- ^ New file to create from patch
        -> IO ()
bspatch _oldf _patchf _newf = do
  o <- S.readFile _oldf
  p <- S.readFile _patchf
  maybe (return ()) (S.writeFile _newf) (patch o p)

--
-- Utilities
--

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

--
-- FFI
--

type Off = (#type off_t)
type SSize = (#type ssize_t)

foreign import ccall unsafe "bsdiff"
  c_bsdiff :: Ptr CChar -> Off
           -> Ptr CChar -> Off
           -> Ptr Word8
           -> IO Int

foreign import ccall unsafe "bspatch_newsize"
  c_bspatch_newsize :: Ptr CChar -> SSize

foreign import ccall unsafe "bspatch"
  c_bspatch :: Ptr CChar -> SSize
            -> Ptr CChar -> SSize
            -> Ptr Word8
            -> IO Int

