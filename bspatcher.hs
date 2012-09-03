{-# LANGUAGE CPP #-}
--
-- bspatcher - a bsdiff clone, under GPLv2 or later
-- Copyright (C) 2012, Austin Seipp
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--
module Main
       ( main -- :: IO ()
       ) where

import System.Environment (getArgs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.BSDiff

-- Compression algorithms
import qualified Codec.Compression.LZ4 as LZ4
import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Compression.QuickLZ as QuickLZ
#ifdef SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif


main :: IO ()
main = getArgs >>= go
  where go :: [String] -> IO ()
        go ["-h"]         = usage

        go ["gen",i,o,p]  = bsdiff i o p
        go ["app",o,p,n]  = bspatch o p n

        go ("-lz4":xs)    = patcher lz4 xs
        go ("-zlib":xs)   = patcher zlib xs
        go ("-gzip":xs)   = patcher gzip xs
        go ("-bz2":xs)    = patcher bz2 xs
        go ("-qlz":xs)    = patcher qlz xs
#ifdef SNAPPY
        go ("-snappy":xs) = patcher snappy xs
#endif
        go _              = usage

        patcher (f,_) ["gen",i,o,p] = bsdiff' f i o p
        patcher (_,g) ["app",o,p,n] = bspatch' g o p n
        patcher _ _                 = usage

        -- These define our compressors and our decompressors.
        lz4  = (LZ4.compressPlusHC, LZ4.decompressPlusHC)
        zlib = (lift (Zlib.compressWith zbest), lift Zlib.decompress)
        gzip = (lift (GZip.compressWith gbest), lift GZip.decompress)
        bz2  = (lift BZip.compress, lift BZip.decompress)
        qlz  = (Just . QuickLZ.compress, Just . QuickLZ.decompress)
#ifdef SNAPPY
        snappy = (Just . Snappy.compress, Just . Snappy.decompress)
#endif

        zbest = Zlib.defaultCompressParams { Zlib.compressLevel = Zlib.bestCompression }
        gbest = GZip.defaultCompressParams { GZip.compressLevel = GZip.bestCompression }

-- Used to lift the stream compressors from lazy to strict.
lift :: (L.ByteString -> L.ByteString) -> B.ByteString -> Maybe B.ByteString
lift f = Just . B.concat . L.toChunks . f . L.fromChunks . (:[])

usage :: IO ()
usage = putStrLn $ "usage:\n\n" ++
                   "Generate patch:\n" ++
                   "\t$ bspatcher [-comp] gen <v1> <v2> <patch>\n" ++
                   "Apply patch:\n" ++
                   "\t$ bspatcher [-comp] app <v1> <patch> <v2>\n\n" ++
                   "The [-comp] option enables compression, and can be one of:\n\n" ++
                   "\t-lz4\t -- lz4 compression\n" ++
                   "\t-zlib\t -- zlib compression\n" ++
                   "\t-gzip\t -- gzip compression\n" ++
                   "\t-bz2\t -- bzip compression\n" ++
                   "\t-qlz\t -- quicklz compression\n"
#ifdef SNAPPY
                   ++ "\t-snappy\t -- snappy compression\n"
#endif

