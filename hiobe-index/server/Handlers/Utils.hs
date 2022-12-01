module Handlers.Utils where

import Data.Text
import Data.Text.Encoding

import Data.Text.Lazy qualified as Lazy

import Database.SQLite.Simple
import Network.Wai
import Web.Scotty.Trans

import Database
import State

trackPath :: ActionT Lazy.Text HiobeM ()
trackPath = do
    p <- decodeUtf8 . rawPathInfo <$> request
    hiobeM $ putReq p

trackLang :: Text -> ActionT Lazy.Text HiobeM ()
trackLang l = hiobeM $ putLang l

withDB :: (Connection -> IO a) -> ActionT Lazy.Text HiobeM a
withDB g = hiobeM $ runDB g
