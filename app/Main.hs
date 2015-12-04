module Main where

import Rest
import Network.Wai.Handler.Launch (run)

main :: IO ()
main = app >>= run

