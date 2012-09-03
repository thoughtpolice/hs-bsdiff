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
import qualified Codec.Compression.Snappy as Snappy


main :: IO ()
main = getArgs >>= go
  where go :: [String] -> IO ()
        go ["-h"]             = usage

        go ["gen",i,o,p]      = bsdiff i o p
        go ["app",o,p,n]      = bspatch o p n

        go ("-lz4":xs)    = patcher LZ4.compressPlusHC xs
        go ("-zlib":xs)   = patcher (lift $ Zlib.compressWith zbest) xs
        go ("-gzip":xs)   = patcher (lift $ GZip.compressWith gbest) xs
        go ("-bz2":xs)    = patcher (lift BZip.compress) xs
        go ("-qlz":xs)    = patcher (Just . QuickLZ.compress) xs
        go ("-snappy":xs) = patcher (Just . Snappy.compress) xs
        go _                  = usage

        patcher f ["gen",i,o,p] = bsdiff' f i o p
        patcher f ["app",o,p,n] = bspatch' f o p n
        patcher _ _             = usage

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
                   "\t-qlz\t -- quicklz compression\n" ++
                   "\t-snappy\t -- snappy compression\n"