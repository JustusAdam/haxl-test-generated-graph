{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import Lib
import Text.Printf
import Control.Exception
import System.CPUTime
import Haxl.Prelude
import Haxl.Core (runHaxl, initEnv, stateEmpty, stateSet)




time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v


somethingelse = return . length


runTest :: IO Int
runTest =
    let
        stateStore = stateSet DataSourceState stateEmpty
    in do
    myEnv <- initEnv stateStore ()
    runHaxl myEnv $ do
        six <- datasource "foo" [1000]
        seven <- somethingelse [1000]
        eight <- datasource "foo" [1000]
        nine <- somethingelse [1000]
        three <- somethingelse [1000, seven, nine]
        four <- datasource "foo" [1000, seven, eight]
        five <- datasource "foo" [1000, nine]
        one <- datasource "foo" [1000, three, four, nine]
        two <- somethingelse [1000, six, five]
        somethingelse [1, one, two]


main = do
    putStrLn "Starting..."
    time $ runTest >>= print
    putStrLn "Done."
