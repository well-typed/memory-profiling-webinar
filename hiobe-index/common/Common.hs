{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, intercalate, toLower)
import GHC.Generics (Generic)

data SurveyResponse =
      SurveyResponse
      { haveWorkedWith :: [Text]
      , wantToWorkWith :: [Text]
      , yearlyComp     :: Maybe Integer
      }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

data SurveyResult =
      SurveyResult
      { message    :: Text
      , responseId :: Integer
      }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

toResponseFormat :: [Text] -> Maybe Text
toResponseFormat = fixEmpty . toLower . intercalate ";"
  where
    fixEmpty "" = Nothing
    fixEmpty t  = Just t
