{-# LANGUAGE OverloadedStrings #-}
module Rest where

import Network.Wai
import Web.Scotty

app :: IO Application
app = scottyApp bitterXylophoneApp

bitterXylophoneApp :: ScottyM ()
bitterXylophoneApp = do
  get "/" $ do
    html $ "<h1>Hello, World!</h1><p>...from Scotty</p>"

