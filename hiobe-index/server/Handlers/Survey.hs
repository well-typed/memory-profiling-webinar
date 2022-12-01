module Handlers.Survey where

import Control.Monad

import Data.Text.Lazy qualified as Lazy

import Web.Scotty.Trans

import Common
import Database
import Handlers.Utils
import State

handlers :: ScottyT Lazy.Text HiobeM ()
handlers = post "/survey/respond" $ do
    trackPath
    resp <- jsonData @SurveyResponse
    forM_ (haveWorkedWith resp) trackLang
    forM_ (wantToWorkWith resp) trackLang
    respId <- hiobeM . runDB $ \conn -> insertResponse conn resp
    json (SurveyResult "Thanks!" respId)
