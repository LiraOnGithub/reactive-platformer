module Main where

import qualified Sdl
import qualified EventNetwork

main :: IO ()
main = Sdl.withSdl EventNetwork.run
