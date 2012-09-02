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
import Data.ByteString

-- | Creates a delta between two 'ByteString's. Can be applied
-- to the old version using 'patch', resuling in the new version.
diff :: ByteString       -- ^ Old version
     -> ByteString       -- ^ New version
     -> Maybe ByteString -- ^ Resulting delta
diff _old _new = Nothing
{-# INLINEABLE diff #-}

-- | Apply a binary patch to input, resuling in new version of input.
patch :: ByteString       -- ^ Old version
      -> ByteString       -- ^ Delta produced by 'diff'
      -> Maybe ByteString -- ^ New version
patch _old _patch = Nothing
{-# INLINEABLE patch #-}

-- | Create a patch file, based on an old version of a file
-- and a new version.
bsdiff :: FilePath -- ^ Old file
       -> FilePath -- ^ New file
       -> FilePath -- ^ Patch file to create
       -> IO ()
bsdiff _oldf _newf _patchf = return ()

-- | Apply a patch file to an old version of a file, resulting
-- in a new version.
bspatch :: FilePath -- ^ Old file
        -> FilePath -- ^ Patch file
        -> FilePath -- ^ New file to create from patch
        -> IO ()
bspatch _oldf _patchf _newf = return ()

