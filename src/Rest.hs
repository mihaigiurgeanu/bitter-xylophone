{-# LANGUAGE OverloadedStrings #-}
module Rest where

import Network.Wai
import Network.HTTP.Types

app :: Application
app _ response = response $ responseLBS status200 [("Content-Type", "text/html")] "<h1>Hello World!</h1>"
