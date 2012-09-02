module Main
       ( main -- :: IO ()
       ) where

import System.Environment (getArgs)
import Data.ByteString.BSDiff

main :: IO ()
main = getArgs >>= go
  where go ["diff",inf,out,patch]  = bsdiff inf out patch
        go ["patch",inf,patch,out] = bspatch inf patch out
        go _                         = return ()
