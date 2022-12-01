module Handlers.Languages where

import Data.Text.Lazy qualified as Lazy

import Web.Scotty.Trans

import Database
import Handlers.Utils
import State

handlers :: ScottyT Lazy.Text HiobeM ()
handlers = do
    get "/languages/list" $ do
      trackPath
      langs <- withDB $ \conn ->
        listLangs conn
      json langs

    get "/languages/count/have/:lang" $ do
      trackPath
      lang <- param "lang"
      trackLang lang
      count <- withDB $ \conn ->
        countLang conn LangHave lang
      text . Lazy.pack $ show count

    get "/languages/count/want/:lang" $ do
      trackPath
      lang <- param "lang"
      trackLang lang
      count <- withDB $ \conn ->
        countLang conn LangWant lang
      text . Lazy.pack $ show count

    get "/languages/hist/have" $ do
      trackPath
      langCounts <- withDB $ \conn ->
        buildHist conn LangHave
      json langCounts

    get "/languages/hist/want" $ do
      trackPath
      langCounts <- withDB $ \conn ->
        buildHist conn LangWant
      json langCounts
