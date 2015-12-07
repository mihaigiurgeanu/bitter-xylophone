{-# LANGUAGE OverloadedStrings #-}
module Rest where

import Network.Wai
import Network.Wai.Middleware.Static
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)

app :: IO Application
app = scottyApp bitterXylophoneApp

bitterXylophoneApp :: ScottyM ()
bitterXylophoneApp = do
  middleware $ staticPolicy (noDots >-> addBase "resources")
  get "/" $ do
    redirect "/index.html"
  post (regex "/files/(.*)") $ do
    filename <- param "1"
    command <- (param "command") :: ActionM Text
    processFileCommand filename command

processFileCommand :: Text -> Text -> ActionM ()
processFileCommand filename command = processFileCommand' command
  where
    processFileCommand' "execute" = text $ mconcat ["<p>Executing file: <strong>", filename, "</strong></p>"]
    processFileCommand' _ = next

