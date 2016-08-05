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
import           Lib
import           System.CPUTime
import           TestGraphs
import           Text.Printf


data MeasuredGraph = MeasuredGraph
    { nr      :: Int
    , levels  :: Int
    , rounds  :: Int
    , fetches :: Int
    } deriving (Generic, Show, Eq, Ord)


type MeasuredGraphs = [MeasuredGraph]


instance ToJSON MeasuredGraph where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelTo '_' }


time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v


main = do
    results <- for allTests $ \(function, currLevels, index) -> do
        let stateStore = stateSet DataSourceState stateEmpty
        myEnv <- initEnv stateStore ()
        _ <- function myEnv
        stats <- readIORef $ statsRef myEnv
        return $ MeasuredGraph { nr = index
                               , levels = currLevels
                               , fetches = numFetches stats
                               , rounds = numRounds stats
                               }

    B.putStrLn (encode results)
