module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import System.Directory
import System.Environment
import System.Exit

import Data.Text.Lazy qualified as Lazy

import Database.SQLite.Simple
import Web.Scotty.Trans

import Handlers
import State

import GHC.Debug.Stub

-- | Serves truth
truth :: ScottyT Lazy.Text HiobeM ()
truth =
    get "/truth" $
      text "HIOBE: Haskell Is Obviously Better at Everything"

main :: IO ()
main = withGhcDebug $ do
    -- get a database path from command line args
    -- default to database/hiobe.db or hiobe-index/database/hiobe.db
    dbPaths <- getArgs >>= \case
      (f:_) -> return [f]
      _     -> return ["database/hiobe.db", "hiobe-index/database/hiobe.db"]
    dbExists <- or <$> mapM doesFileExist dbPaths
    unless dbExists $ do
      putStrLn $
           "hiobe-index-server: couldn't find database file.\n\n"
        ++ "    Usage: hiobe-index-server DATABASE_FILE (defaults to "
        ++ "database/hiobe.db or hiobe-index/database/hiobe.db)\n"
      exitFailure

    dbPath <- head <$> filterM doesFileExist dbPaths

    -- open our singular database connection, wrap it in an MVar,
    -- initialise app state
    withConnection dbPath $ \conn -> do
      db <- newMVar conn
      sync <- newTVarIO (initState db)

      let runActionToIO m = runReaderT (runHiobeM m) sync

      scottyT 3000 runActionToIO hiobeApp

hiobeApp :: ScottyT Lazy.Text HiobeM ()
hiobeApp = do
    truth
    handlers
