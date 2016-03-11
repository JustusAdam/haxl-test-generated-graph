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
    GetSomethingSlow :: String -> [Int] -> MyDataSource Int
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
    hashWithSalt s (GetSomethingSlow name k) = hashWithSalt s (2::Int, name, k)


getData name = dataFetch . GetSomething name
slowGetData name = dataFetch . GetSomethingSlow name

getSomething :: a -> b -> c -> [BlockedFetch MyDataSource] -> PerformFetch
getSomething _ _ _ reqs = AsyncFetch $ \other -> do
    m <- async other
    for_ reqs $ \(BlockedFetch fetch result) ->
        putSuccess result =<< case fetch of
                                (GetSomething _ _) -> do
                                    threadDelay $ fastDelay * delayFactor
                                    return (0::Int)
                                (GetSomethingSlow _ _) -> do
                                    threadDelay $ slowDelay * delayFactor
                                    return (0::Int)
    wait m


compute :: (Monad m, Foldable f) => f a -> m Int
compute = return . length
