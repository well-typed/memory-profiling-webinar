{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad
import System.IO

import Data.Map        qualified as LazyMap
import Data.Map.Strict qualified as StrictMap

import GHC.Debug.Stub

main :: IO ()
main = withGhcDebug $ do
    putStr "Enter a number: "
    hFlush stdout
    !num <- readLn :: IO Integer

    -- example 0 is a list
    let xs = map (+1) [0..num]
    saveAndPause "example0" [Box xs]

    -- example 1 is the same list, forced to WHNF by the bang pattern
    let !example1 = xs
    saveAndPause "example1" [Box example1]

    -- example 2 is the length of the list
    let example2 = length xs
    saveAndPause "example2" [Box example2]

    -- example 3 is the length, forced to WHNF. What do we expect this to do to
    -- the list?
    let !example3 = length xs
    saveAndPause "example3" [Box example3, Box xs]

    -- A lazy map
    let example4 = LazyMap.fromList $ [(n-1, n+1) | n <- [0..num]]
    saveAndPause "example4" [Box example4]

    -- A lazy map, forced to WHNF
    let !example5 = LazyMap.fromList $ [(n-1, n+1) | n <- [0..num]]
    saveAndPause "example5" [Box example5]

    -- A strict map, unforced
    let example6 = StrictMap.fromList $ [(n-1, n+1) | n <- [0..num]]
    saveAndPause "example6" [Box example6]

    -- A strict map, forced
    let !example7 = StrictMap.fromList $ [(n-1, n+1) | n <- [0..num]]
    saveAndPause "example7" [Box example7]

    -- A strict map of Maybes, forced
    let !example8 = StrictMap.fromList $
          [(n-1, integerToMaybe $ n+1) | n <- [0..num]]
    saveAndPause "example8" [Box example8]

    -- A strict map of strict Maybes, forced
    let !example9 = StrictMap.fromList $
          [(n-1, integerToStrictMaybe $ n+1) | n <- [0..num]]
    saveAndPause "example9" [Box example9]

    putStrLn "Done"

integerToMaybe :: Integer -> Maybe String
integerToMaybe 42 = Nothing
integerToMaybe n  = Just $ "was not 42... was " ++ show n

data Maybe' a = Nothing' | Just' !a

integerToStrictMaybe :: Integer -> Maybe' String
integerToStrictMaybe 42 = Nothing'
integerToStrictMaybe n  = Just' $ "was not 42... was " ++ show n


saveAndPause :: String -> [Box] -> IO ()
saveAndPause desc boxes = do
    saveClosures boxes
    putStr $ "Paused: " ++ desc ++ " (hit enter to continue)"
    hFlush stdout
    void getLine
