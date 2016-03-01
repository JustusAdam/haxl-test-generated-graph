{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Lib
    ( datasource
    , State(DataSourceState)
    ) where


import           Control.Concurrent (threadDelay)
import           Data.Foldable      (for_)
import           Data.Hashable
import           Data.Typeable
import           Haxl.Core
import           Haxl.Prelude
import           Text.Printf


data MyDataSource a where
    GetSomething :: String -> [Int] -> MyDataSource Int
    deriving Typeable


deriving instance Eq (MyDataSource a)
deriving instance Show (MyDataSource a)
instance Show1 MyDataSource where show1 = show


instance DataSourceName MyDataSource where
    dataSourceName _ = "MyDataSource"


instance StateKey MyDataSource where
    data State MyDataSource = DataSourceState


instance DataSource u MyDataSource where
    fetch = getSomething

instance Hashable (MyDataSource a) where
    hashWithSalt s (GetSomething name k) = hashWithSalt s (0::Int, name, k)


datasource name = dataFetch . GetSomething name

getSomething :: a -> b -> c -> [BlockedFetch MyDataSource] -> PerformFetch
getSomething _ _ _ reqs = SyncFetch $ do
    printf "Fetch round with requests %v\n" (show (map (\(BlockedFetch fetch result) -> show fetch) reqs))
    for_ reqs $ \(BlockedFetch fetch result) ->
        putSuccess result $ case fetch of
                                (GetSomething _ _) -> (0::Int)
