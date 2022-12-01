{-# LANGUAGE DataKinds #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Text (Text)
import System.Random

import Data.Text qualified as T

import Network.HTTP.Client (Response (responseStatus))
import Network.HTTP.Req

import Common

main :: IO ()
main = do
    langs <- getLangs
    let enhancedLangs = langs ++ replicate (length langs `div` 3) "haskell"
        enhancedLangsNoSlash =  enhancedLangs \\ ["html/css","bash/shell"]
    mapM_ (\n -> forkIO (submitter n enhancedLangs)) [1..15]
    mapM_ (\n -> forkIO (requester n enhancedLangsNoSlash)) [1..15]
    void getLine

getLangs :: IO [Text]
getLangs = runReq defaultHttpConfig $
    responseBody <$>
      req GET
        (http "localhost" /: "languages" /: "list")
        NoReqBody
        jsonResponse
        (port 3000)

submit :: SurveyResponse -> IO SurveyResult
submit resp = runReq defaultHttpConfig $
    responseBody <$>
      req POST
        (http "localhost" /: "survey" /: "respond")
        (ReqBodyJson resp)
        jsonResponse
        (port 3000)

submitter :: Integer -> [Text] -> IO ()
submitter myId langs = do
    wait <- randomRIO (1_000_000,3_000_000)
    threadDelay wait
    haveLangs <- pickLangs
    wantLangs <- (\\ haveLangs) <$> pickLangs
    salary    <- calcSalary haveLangs
    result    <- submit $ SurveyResponse haveLangs wantLangs (Just salary)
    putStrLn $
         "Submitter " <> show myId <> " submitted response ID " <> show (responseId result) <> ":"
      <> "\n    Has langs:   "  <> show haveLangs
      <> "\n    Wants langs: "  <> show wantLangs
      <> "\n    Salary:      $" <> show salary
    submitter myId langs
  where
    pickLangs :: IO [Text]
    pickLangs = nub <$> do
      n <- randomRIO (3,12)
      replicateM n (pick langs)

    calcSalary :: [Text] -> IO Integer
    calcSalary ls = randomRIO range
      where range | "haskell" `elem` ls = (100_000,6_000_000)
                  | otherwise           = (15_000 ,40_000)

requester :: Integer -> [Text] -> IO ()
requester myId langs = do
    wait <- randomRIO (1_000_000,3_000_000)
    threadDelay wait
    lang <- pick langs
    url <- pick validUrls <*> pure lang
    status <- responseStatus . toVanillaResponse <$> runReq defaultHttpConfig (toReq url)
    putStrLn $
         "Requester " <> show myId <> " requested path " <> T.unpack (renderUrl url) <> ":"
      <> "\n    Status:   "  <> show status
    requester myId langs
  where
    validUrls :: [Text -> Url 'Http]
    validUrls =
      [ \_ -> http "localhost" /: "truth"
      , \_ -> http "localhost" /: "languages" /: "list"
      , \l -> http "localhost" /: "languages" /: "count" /: "have" /: l
      , \l -> http "localhost" /: "languages" /: "count" /: "want" /: l
      , \_ -> http "localhost" /: "languages" /: "hist" /: "have"
      , \_ -> http "localhost" /: "languages" /: "hist" /: "want"
      ]
    toReq u = req GET u NoReqBody ignoreResponse (port 3000)

pick :: [a] -> IO a
pick xs = (xs !!) <$> randomRIO (0, length xs - 1)
