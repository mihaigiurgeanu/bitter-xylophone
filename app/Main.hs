module Main where

import Rest
import Network.Wai.Handler.Webkit (run)

main :: IO ()
main = run "bitter-xylophone" app

