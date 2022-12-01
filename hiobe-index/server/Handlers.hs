module Handlers (handlers) where

import Data.Text.Lazy qualified as Lazy

import Web.Scotty.Trans

import State

import Handlers.Languages qualified as Languages
import Handlers.Survey    qualified as Survey

handlers :: ScottyT Lazy.Text HiobeM ()
handlers = mconcat [
      Languages.handlers
    , Survey.handlers
    ]
