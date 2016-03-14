#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module TimeTests where


import Shelly
import qualified Data.Text as T
import Control.Monad
import Data.Monoid
import Prelude hiding (FilePath)
import Text.Printf
import Data.Foldable (for_)


default (T.Text)


graphGenerationBinaryLocation :: FilePath
graphGenerationBinaryLocation = "../rand-code-graph/dist/build/random-level-graphs/random-level-graphs"


outputLocation :: FilePath
outputLocation = "results"


graphsToGenerate :: Int
graphsToGenerate = 5


maxLevel :: Int
maxLevel = 10


seed :: T.Text
seed = "12345"


formatSeconds :: Int -> String
formatSeconds n
    | n > 60 = formatMinutes (n `div` 60) ++ if n `mod` 60 == 0 then "" else " and " ++ show (n `mod` 60) ++ " seconds"
    | otherwise = show n ++ " seconds"
    where
        formatMinutes :: Int -> String
        formatMinutes n
            | n > 60 = formatHours (n `div` 60) ++ if n `mod` 60 == 0 then "" else ", " ++ show (n `mod` 60) ++ " minutes"
            | otherwise = show n ++ " minutes"
        formatHours :: Int -> String
        formatHours = (++ " hours" ) . show


main :: IO ()
main = shelly $ print_stdout False $ escaping False $ do
    mkdir_p outputLocation
    mkdir_p "generated"

    run "cabal" ["install", "--only-dependencies"]

    echo $ T.pack $ printf "Building and testing %i graphs" (graphsToGenerate * maxLevel)

    (generationTime, _) <- time $ run
        graphGenerationBinaryLocation
        [ "-L", "HaskellDoApp"
        , "-p", "resources/SlowPreamble.hs"
        , "-o", "generated/SlowTestGraphs.hs"
        , "-l", T.pack $ show maxLevel
        , "-n", T.pack $ show (graphsToGenerate * maxLevel)
        , "-s", seed
        , "-S"
        ]
    echo $ T.pack $ printf "Generated %i graphs in %s" (graphsToGenerate * maxLevel) (formatSeconds $ ceiling generationTime)
    (compilationTime, _) <- time $ run "cabal" ["build"]
    echo $ T.pack $ printf "Compiled test program in %s" (formatSeconds $ ceiling compilationTime)
    (runTime, _) <- time $ run "dist/build/haxl-test-execution-time/haxl-test-execution-time" [] >>=
        writefile (outputLocation </> "haskell-timed.json")
    echo $ T.pack $ printf "Ran program in %s" (formatSeconds $ ceiling runTime)
