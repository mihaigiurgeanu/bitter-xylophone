{-# LANGUAGE OverloadedStrings #-}
module Rest where

import Network.Wai
import Network.Wai.Middleware.Static
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)
import qualified Data.Text as T (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, Value (Object), object, (.=), (.:))

import Process (runCommand, getNextUpdate, terminateCommand, startAppState, AppState, ExecRequest(..))

app :: IO Application
app = do
  appState <- startAppState
  scottyApp $ bitterXylophoneApp appState

data RESTResponse = ProcessExec T.Text | StateUpdate Int [T.Text]
              deriving Show

instance ToJSON RESTResponse where
  toJSON (ProcessExec uuid) = object ["uuid" .= uuid]
  toJSON (StateUpdate crt uuids) = object ["crt" .= crt, "uuids" .= uuids]



instance FromJSON ExecRequest where
  parseJSON (Object v) = ExecFile <$>
                         v .: "command" <*>
                         v .: "args"
  parseJSON _ = fail "Expected json object {command: \"filePath\", args: [\"arg1\", \"arg2\"]}"


bitterXylophoneApp :: AppState -> ScottyM ()
bitterXylophoneApp state = do
  middleware $ staticPolicy (noDots >-> addBase "resources")
  get "/" $ do
    redirect "/index.html"
  post "/processes/:uuid" $ do
    uuid <- param "uuid"
    liftIO $ terminateCommand state uuid
  post (regex "/processes") $ do
    execRequest <- jsonData
    uuid <- liftIO $ runCommand state execRequest
    json $ ProcessExec uuid
  get "/processes" $ do
    known <- param "known"
    (crt, uuids) <- liftIO $ getNextUpdate state known
    json $ StateUpdate crt uuids    



