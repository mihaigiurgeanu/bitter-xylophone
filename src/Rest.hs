{-# LANGUAGE OverloadedStrings #-}
module Rest where

import Network.Wai
import Network.Wai.Middleware.Static
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map (singleton)

import Process (runCommand, getNextUpdate, terminateCommand, startAppState, AppState)

app :: IO Application
app = do
  appState <- startAppState
  scottyApp $ bitterXylophoneApp appState

bitterXylophoneApp :: AppState -> ScottyM ()
bitterXylophoneApp state = do
  middleware $ staticPolicy (noDots >-> addBase "resources")
  get "/" $ do
    redirect "/index.html"
  post (regex "/processes") $ do
    command <- (param "command") :: ActionM Text
    uuid <- liftIO $ runCommand state command
    json $ Map.singleton ("uuid" :: Text) uuid
  get "/processes" $ do
    known <- param "known"
    uuids <- liftIO $ getNextUpdate state known
    json $ uuids
  post "/processes/:uuid" $ do
    uuid <- param "uuid"
    liftIO $ terminateCommand state uuid
    



