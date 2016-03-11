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
import           SlowLib
import           System.CPUTime
import           SlowTestGraphs
import           Text.Printf
import Data.Time


data TimedGraph = TimedGraph
    { levels :: Int
    , time :: Double
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON TimedGraph where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelTo '_' }


timeExec :: IO t -> IO (t, Double)
timeExec a = do
    start <- getCurrentTime
    v <- a
    end   <- getCurrentTime
    let diff = realToFrac $ diffUTCTime end start
    return (v, diff * 1000)


main = do
    results <- for allTests $ \(function, currLevels, index) -> do
        let stateStore = stateSet DataSourceState stateEmpty
        myEnv <- initEnv stateStore ()
        (_, execTime) <- timeExec $ function myEnv
        return $ TimedGraph { levels = currLevels
                            , time = execTime
                            }

    B.putStrLn (encode results)
