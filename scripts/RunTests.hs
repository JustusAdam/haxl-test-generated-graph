#!/usr/bin/env runhaskell
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module RunTests where


import           Control.Monad
import           Data.Foldable (for_)
import           Data.Monoid
import qualified Data.Text     as T
import           Prelude       hiding (FilePath)
import           Shelly
import           Text.Printf


default (T.Text)


graphGenerationBinaryLocation :: FilePath
graphGenerationBinaryLocation = "../rand-code-graph/dist/build/random-level-graphs/random-level-graphs"


outputLocation :: FilePath
outputLocation = "results"


graphsToGenerate :: Int
graphsToGenerate = 20


maxLevel :: Int
maxLevel = 20


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
        [ "-L", "HaskellDo"
        , "-p", "resources/Preamble.hs"
        , "-o", "generated/TestGraphs.hs"
        , "-l", T.pack $ show maxLevel
        , "-n", T.pack $ show (graphsToGenerate * maxLevel)
        , "-s", seed
        ]
    echo $ T.pack $ printf "Generated %i graphs in %s" (graphsToGenerate * maxLevel) (formatSeconds $ ceiling generationTime)
    (compilationTime, _) <- time $ run "cabal" ["build"]
    echo $ T.pack $ printf "Compiled test program in %s" (formatSeconds $ ceiling compilationTime)
    (runTime, _) <- time $ run "dist/build/haxl-test-generated-graph-exe/haxl-test-generated-graph-exe" [] >>=
        writefile (outputLocation </> "haskell.json")
    echo $ T.pack $ printf "Ran program in %s" (formatSeconds $ ceiling runTime)

    (generationTime, _) <- time $ run
        graphGenerationBinaryLocation
        [ "-L", "HaskellDoApp"
        , "-p", "resources/Preamble.hs"
        , "-o", "generated/TestGraphs.hs"
        , "-l", T.pack $ show maxLevel
        , "-n", T.pack $ show (graphsToGenerate * maxLevel)
        , "-s", seed
        ]
    echo $ T.pack $ printf "Generated %i graphs in %s" (graphsToGenerate * maxLevel) (formatSeconds $ ceiling generationTime)
    (compilationTime, _) <- time $ run "cabal" ["build"]
    echo $ T.pack $ printf "Compiled test program in %s" (formatSeconds $ ceiling compilationTime)
    (runTime, _) <- time $ run "dist/build/haxl-test-generated-graph-exe/haxl-test-generated-graph-exe" [] >>=
        writefile (outputLocation </> "haskell-applicative.json")
    echo $ T.pack $ printf "Ran program in %s" (formatSeconds $ ceiling runTime)
