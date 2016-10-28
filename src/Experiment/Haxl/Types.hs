{-# LANGUAGE TemplateHaskell #-}
module Experiment.Haxl.Types where


import           ClassyPrelude
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Experiment.Haxl.Util



data GenConf = MkGenConf
    { lang           :: Text
    , numLevels      :: Int
    , numGraphs      :: Int
    , seed           :: Maybe Int
    , prctFuns       :: Maybe Double
    , prctMaps       :: Maybe Double
    , prctIfs        :: Maybe Double
    , prctSlow       :: Maybe Double
    , slowDataSource :: Maybe Bool
    , inlineIf       :: Maybe Bool
    , levelWidth     :: Maybe Int
    } deriving (Show, Eq, Ord)


deriveJSON gconfOptions ''GenConf


data MeasuredGraph = MeasuredGraph
    { nr      :: Maybe Int
    , levels  :: Int
    , rounds  :: Int
    , fetches :: Int
    , genConf :: Maybe GenConf
    , time    :: Double
    } deriving (Show, Eq, Ord)


type MeasuredGraphs = [MeasuredGraph]

deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' } ''MeasuredGraph
