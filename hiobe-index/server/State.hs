{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE BangPatterns               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics
import GHC.Stack
import System.IO.Unsafe

import Data.Map.Strict qualified as Map

import GHC.Debug.Stub
import NoThunks.Class

import Database.SQLite.Simple

newtype HiobeM a = HiobeM { runHiobeM :: ReaderT (TVar HiobeState) IO a }
  deriving newtype
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  , MonadReader (TVar HiobeState)
  )

data HiobeState =
    HiobeState
    { dbConn          :: MVar Connection
    , reqCount        :: Map Text Integer
    , langEngagements :: Map Text Integer
    }
  deriving (Generic, NoThunks)

deriving via OnlyCheckWhnf Connection instance NoThunks Connection

initState :: MVar Connection -> HiobeState
initState dbConn =
    HiobeState
    { dbConn          = dbConn
    , langEngagements = Map.empty
    , reqCount        = Map.empty
    }

hiobeM :: MonadTrans t => HiobeM a -> t HiobeM a
hiobeM = lift

gets :: (HiobeState -> a) -> HiobeM a
gets g = do
    s <- asks (liftIO . readTVarIO)
    g <$> s

modify :: HasCallStack => (HiobeState -> HiobeState) -> HiobeM ()
modify g = do
    s <- ask
    liftIO . atomically $ modifyTVar s g

modify' :: (HiobeState -> HiobeState) -> HiobeM ()
modify' g = do
    s <- ask
    liftIO . atomically $ modifyTVar' s g

putLang :: Text -> HiobeM ()
putLang l =
    modify $ \HiobeState{..} -> do
      HiobeState dbConn reqCount (Map.insertWith (+) l 1 langEngagements)

putReq :: Text -> HiobeM ()
putReq p = do
    modify $ \HiobeState{..} ->
      HiobeState dbConn (Map.insertWith (+) p 1 reqCount) langEngagements

unsafeCheckThunks :: NoThunks a => a -> a
unsafeCheckThunks !x = case unsafeNoThunks x of
    Nothing    -> x
    Just thunk ->
      case unsafePerformIO $ do
        putStrLn $
               "THUNK ALERT:\n    "
            ++ show thunk ++ "\n    "
            ++ show callStack ++ "\n"
            ++ "pausing for ghc-debug analysis"
        saveClosures [Box x]
        void getLine
      of
        () -> x
