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
-- This module exports some simple interfaces to bsdiff, which allows you to
-- create and apply binary patches to arbitrary 'ByteString's or files (for
-- example, if you want to do incremental software updates to users.)
--
-- The interface is based on minibsdiff, which is a portable derivative of
-- bsdiff v4.3.  See <http://github.com/thoughtpolice/minibsdiff> for
-- information.
--
module Data.ByteString.BSDiff
       ( -- * ByteString interface
         diff   -- :: ByteString -> ByteString -> Maybe ByteString
       , patch  -- :: ByteString -> ByteString -> Maybe ByteString

         -- * File interface
       , bsdiff   -- :: FilePath -> FilePath -> FilePath -> IO ()
       , bspatch  -- :: FilePath -> FilePath -> FilePath -> IO ()

         -- * Extended file interface
       , bsdiff'  -- :: (ByteString -> ByteString) -> FilePath -> FilePath -> FilePath -> IO ()
       , bspatch' -- :: (ByteString -> ByteString) -> FilePath -> FilePath -> FilePath -> IO ()
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


-----------------------------------------------------------------------------
-- ByteString interface

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
        let sz = fi (news + olds + 1024) -- Just to be safe
        r <- SI.createAndTrim sz $ \out -> do
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


-----------------------------------------------------------------------------
-- File interface

-- | Create a patch file, based on an old version of a file
-- and a new version. Does nothing to the patch before writing
-- it to a file. Defined as:
--
-- > bsdiff = bsdiff' (Just . id)
--
bsdiff :: FilePath -- ^ Old file
       -> FilePath -- ^ New file
       -> FilePath -- ^ Patch file to create
       -> IO ()
bsdiff = bsdiff' (Just . id)

-- | Apply a patch file to an old version of a file, resulting
-- in a new version. Does nothing to the patch before applying
-- it. Defined as:
--
-- > bspatch = bspatch' (Just . id)
--
bspatch :: FilePath -- ^ Old file
        -> FilePath -- ^ Patch file
        -> FilePath -- ^ New file to create from patch
        -> IO ()
bspatch = bspatch' (Just . id)


-----------------------------------------------------------------------------
-- Extended file interface

-- | Create a patch file, based on an old version of a file
-- and a new version. You can also apply a function to the patch
-- before it gets written to a file (for example, compress it.)
--
-- Ignores errors.
bsdiff' :: (ByteString -> Maybe ByteString) -- ^ Transformation function
        -> FilePath -- ^ Old file
        -> FilePath -- ^ New file
        -> FilePath -- ^ Patch file to create
        -> IO ()
bsdiff' f old new pat = do
  o <- S.readFile old
  n <- S.readFile new
  maybe (return ()) (S.writeFile pat) (diff o n >>= f)

-- | Apply a patch file to an old version of a file, resulting
-- in a new version. You can also apply a function to the patch
-- after it is read from the file, and before it is applied (for example,
-- decompress it.)
--
-- Ignores errors.
bspatch' :: (ByteString -> Maybe ByteString) -- ^ Transformation function
         -> FilePath -- ^ Old file
         -> FilePath -- ^ Patch file
         -> FilePath -- ^ New file to create from patch
         -> IO ()
bspatch' f old pat new = do
  o <- S.readFile old
  p <- S.readFile pat
  maybe (return ()) (S.writeFile new) (f p >>= patch o)


-----------------------------------------------------------------------------
-- Utilities

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}


-----------------------------------------------------------------------------
-- FFI

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

