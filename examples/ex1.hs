module Main
       ( main -- :: IO ()
       ) where

import Codec.Compression.LZ4 (compressPlusHC, decompressPlusHC)

import System.Environment (getArgs)
import Data.ByteString.BSDiff

main :: IO ()
main = getArgs >>= go
  where go ["-h"]             = usage
        go ["gen",i,o,p]      = bsdiff i o p
        go ["app",o,p,n]      = bspatch o p n
        go ["-c","gen",i,o,p] = bsdiff' compressPlusHC i o p
        go ["-c","app",o,p,n] = bspatch' decompressPlusHC o p n
        go _                  = usage

usage :: IO ()
usage = putStrLn $ "usage:\n\n" ++
                   "Generate patch:\n" ++
                   "\t$ ex1 [-c] gen <v1> <v2> <patch>\n" ++
                   "Apply patch:\n" ++
                   "\t$ ex1 [-c] app <v1> <patch> <v2>\n\n" ++
                   "The '-c' option enables lz4 compression"
