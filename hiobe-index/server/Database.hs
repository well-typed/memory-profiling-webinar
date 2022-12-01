{-# LANGUAGE ViewPatterns #-}

module Database where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Foreign

import Data.List       qualified as List
import Data.Map.Strict qualified as Map
import Data.Set        qualified as Set
import Data.Text       qualified as T

import Database.SQLite.Simple

import Common
import State

runDB :: (Connection -> IO a) -> HiobeM a
runDB f = do
    conn <- gets dbConn
    liftIO $ withMVar conn f

data LangType = LangHave | LangWant

toCol :: LangType -> Column
toCol LangHave = Have
toCol LangWant = Want

data Column = Have | Want | Comp

toQuery :: Column -> Query
toQuery Have = "LanguageHaveWorkedWith"
toQuery Want = "LanguageWantToWorkWith"
toQuery Comp = "ConvertedCompYearly"

-- | Naive @listLangs@ implementation. Does not stream results from
-- database.
listLangs :: Connection -> IO [Text]
listLangs conn = do
    ls <- concatMap langRowToLangs <$> query_ conn q
    return $ List.nub ls
  where
    q :: Query
    q = "SELECT DISTINCT " <> toQuery Have <> " FROM survey_data"

-- | /Interesting/ implementation of @listLangs@, does stream from the database.
listLangsStream :: Connection -> IO [Text]
listLangsStream conn = Set.toList <$>
    fold_ conn q Set.empty
      (\acc r -> pure $ insertAll (langRowToLangs r) acc)
  where
    q :: Query
    q = "SELECT DISTINCT " <> toQuery Have <> " FROM survey_data"

    insertAll :: [Text] -> Set Text -> Set Text
    insertAll xs ss = foldr Set.insert ss xs

-- | Naive @countLang@ implementation, doesn't stream
countLang :: Connection -> LangType -> Text -> IO Integer
countLang conn (toQuery . toCol -> col) lang = do
    ls <- concatMap langRowToLangs <$> query conn q ["%"<>lang<>"%"]
    return $ foldr ((+) . fromBool . (==lang)) 0 ls
  where
    q :: Query
    q = "SELECT " <> col <> " FROM survey_data WHERE " <> col <> " LIKE ?"

-- | /Interesting/ @countLang@ implementation, does stream
countLangStream :: Connection -> LangType -> Text -> IO Integer
countLangStream conn (toQuery . toCol -> col) lang =
    fold conn q ["%"<>lang<>"%"] 0 $
      \acc r -> pure $ acc + fromBool (lang `elem` langRowToLangs r)
  where
    q :: Query
    q = "SELECT " <> col <> " FROM survey_data WHERE " <> col <> " LIKE ?"

-- | Naive @buildHist@ implementation, doesn't stream
buildHist :: Connection -> LangType -> IO (Map Text Integer)
buildHist conn (toQuery . toCol -> col) = do
    ls <- concatMap langRowToLangs <$> query_ conn q
    return $ foldr (\l h -> Map.insertWith (+) l 1 h) Map.empty ls
  where
    q :: Query
    q = "SELECT " <> col <> " FROM survey_data"

-- | /Interesting/ @buildHist@ implementation, does stream
buildHistStream :: Connection -> LangType -> IO (Map Text Integer)
buildHistStream conn (toQuery . toCol -> col) =
    fold_ conn q Map.empty $
      \acc r -> pure $ insertAll (langRowToLangs r) acc
  where
    q :: Query
    q = "SELECT " <> col <> " FROM survey_data"

    insertAll :: [Text] -> Map Text Integer -> Map Text Integer
    insertAll ls h = foldr (\l m -> Map.insertWith (+) l 1 m) h ls

insertResponse :: Connection -> SurveyResponse -> IO Integer
insertResponse conn resp = do
    [[rid]] <- query conn q $ toQueryArgs resp
    return rid
  where
    q :: Query
    q = mconcat
      [ "INSERT INTO survey_data ("
      , Query . T.intercalate "," $ map fromQuery
        [ toQuery Have
        , toQuery Want
        , toQuery Comp
        ]
        , ") VALUES (?,?,?) "
        , "RETURNING ResponseId"
      ]

toQueryArgs :: SurveyResponse -> (Maybe Text, Maybe Text, Maybe Integer)
toQueryArgs resp =
    ( toResponseFormat $ haveWorkedWith resp
    , toResponseFormat $ wantToWorkWith resp
    , yearlyComp resp
    )

langRowToLangs :: [Maybe Text] -> [Text]
langRowToLangs = concatMap (T.splitOn ";") . catMaybes
