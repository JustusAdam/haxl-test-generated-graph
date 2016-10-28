module Main where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Function              ((&))
import           Data.IORef
import           Data.Time
import           Data.Traversable
import           Haxl.Core
import qualified Lib
import qualified SlowLib
import           System.CPUTime
import           TestGraphs
import           Text.Printf
import Experiment.Haxl.Types



type MeasuredGraphs = [MeasuredGraph]


timeExec :: IO t -> IO (t, Double)
timeExec a = do
    start <- getCurrentTime
    v <- a
    end   <- getCurrentTime
    let diff = realToFrac $ diffUTCTime end start
    return (v, diff * 1000)


main = do
    results <- for allTests $ \(function, currLevels, index) -> do
        let stateStore = stateEmpty
                         & stateSet SlowLib.SlowDataSourceState
                         & stateSet SlowLib.DataSourceState
                         & stateSet Lib.DataSourceState
        myEnv <- initEnv stateStore ()
        (_, execTime) <- timeExec $ function myEnv
        stats <- readIORef $ statsRef myEnv
        return MeasuredGraph { nr = Just index
                             , levels = currLevels
                             , fetches = numFetches stats
                             , rounds = numRounds stats
                             , time = execTime
                             , genConf = Nothing
                             }

    B.putStrLn (encode results)
