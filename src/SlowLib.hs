{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module SlowLib where


import           Control.Concurrent (threadDelay)
import           Data.Foldable      (for_)
import           Data.Hashable
import           Data.Typeable
import           Haxl.Core
import           Haxl.Prelude
import           Text.Printf
import Control.Concurrent.Async (async, wait)
import System.IO


fastDelay :: Int
fastDelay = 3
slowDelay :: Int
slowDelay = 10


delayFactor :: Int
delayFactor = 100000


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


getData name = dataFetch . GetSomething name

getSomething :: a -> b -> c -> [BlockedFetch MyDataSource] -> PerformFetch
getSomething _ _ _ reqs = AsyncFetch $ \other -> do
    m <- async other
    threadDelay $ fastDelay * delayFactor
    for_ reqs $ \(BlockedFetch fetch result) -> do
        putSuccess result $ case fetch of
                                (GetSomething _ _) -> (0::Int)
    wait m


data MySlowDataSource a where
    GetSomethingSlow :: String -> [Int] -> MySlowDataSource Int
    deriving Typeable


deriving instance Eq (MySlowDataSource a)
deriving instance Show (MySlowDataSource a)
instance Show1 MySlowDataSource where show1 = show


instance DataSourceName MySlowDataSource where
    dataSourceName _ = "MySlowDataSource"


instance StateKey MySlowDataSource where
    data State MySlowDataSource = SlowDataSourceState


instance DataSource u MySlowDataSource where
    fetch = getSomethingSlow

instance Hashable (MySlowDataSource a) where
    hashWithSalt s (GetSomethingSlow name k) = hashWithSalt s (2::Int, name, k)

slowGetData name = dataFetch . GetSomethingSlow name

getSomethingSlow :: a -> b -> c -> [BlockedFetch MySlowDataSource] -> PerformFetch
getSomethingSlow _ _ _ reqs = AsyncFetch $ \other -> do
    m <- async other
    threadDelay $ slowDelay * delayFactor
    for_ reqs $ \(BlockedFetch fetch result) -> do
        putSuccess result $ case fetch of
                                (GetSomethingSlow _ _) -> (0::Int)
    wait m


compute :: (Monad m, Foldable f) => f a -> m Int
compute = return . length
