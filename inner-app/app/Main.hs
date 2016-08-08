{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.IORef
import           Data.Traversable
import           GHC.Generics
import           Haxl.Core
import qualified Lib
import qualified SlowLib
import           System.CPUTime
import           TestGraphs
import           Text.Printf
import           Data.Time
import Data.Function ((&))



data MeasuredGraph = MeasuredGraph
    { nr      :: Int
    , levels  :: Int
    , rounds  :: Int
    , fetches :: Int
    , time :: Double
    } deriving (Generic, Show, Eq, Ord)


type MeasuredGraphs = [MeasuredGraph]


instance ToJSON MeasuredGraph where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelTo2 '_' }


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
        return $ MeasuredGraph { nr = index
                               , levels = currLevels
                               , fetches = numFetches stats
                               , rounds = numRounds stats
                               , time = execTime
                               }

    B.putStrLn (encode results)
