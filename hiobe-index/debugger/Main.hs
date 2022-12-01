{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import Data.List.NonEmpty (NonEmpty(..))
import System.Environment

import GHC.Debug.Client
import GHC.Debug.Client.Search
import GHC.Debug.Retainers
import GHC.Debug.Snapshot

main :: IO ()
main = getArgs >>= \case
    -- Take a snapshot of the heap and store it in a file for offline debugging
    ["snapshot",snapshotName] -> do
      sock <- getEnv "GHC_DEBUG_SOCKET"
      withDebuggeeConnect sock $
        \d -> makeSnapshot d snapshotName

    -- Run the analysis on the given snapshot
    ["debug",snapshotName] -> do
      putStrLn "debugging away..."
      snapshotRun snapshotName analysis

    -- Run the analysis on a "live" debuggee over a socket
    ["debug"] -> do
      putStrLn "debugging away..."
      sock <- getEnv "GHC_DEBUG_SOCKET"
      withDebuggeeConnect sock analysis

    _ -> putStrLn "bad arguments"

-- This is a simple analysis of the data retained by our HIOBE state
analysis :: Debuggee -> IO ()
analysis d = withPause d $ do
    -- The first part of this analysis counts the number of integers and map
    -- nodes retained by the HiobeState of the debuggee

    -- Create a heap graph of all data retained by the HIOBE state
    g@(HeapGraph (hiobeClosurePtr :| _) _) <- run d $ do
      -- Get the garbage collection roots
      rs <- gcRoots

      -- Find a path from the GC roots to the HIOBE state
      [rets] <- findRetainersOfConstructor (Just 1) rs "HiobeState"

      -- The closure pointer of the HIOBE state
      let hiobeStatePtr = last rets

      -- Build a heap graph of of the memory "underneath" (retained by) the
      -- HIOBE state
      buildHeapGraph Nothing hiobeStatePtr


    let integers = findConstructors "IS" g
        bins = findConstructors "Bin" g

    putStrLn $ "found " ++ show (length integers) ++ " integers"
    putStrLn $ "found " ++ show (length bins) ++ " map nodes\n"

    -- Now we sample some retainer paths of the integers to see what part of the
    -- HiobeState is holding on to them
    retainerPaths <- run d $ do
      rss <- findRetainersOfConstructor Nothing [hiobeClosurePtr] "IS"
      zip (map show [0 :: Int ..]) <$> mapM addLocationToStack rss

    putStrLn "Integer retainer paths:"
    displayRetainerStack retainerPaths
