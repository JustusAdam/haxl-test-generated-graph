#!/usr/bin/env runhaskell
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module RunTests where


import           Control.Monad
import           Data.Foldable (for_)
import           Data.Monoid
import qualified Data.Text     as T
import           Prelude       hiding (FilePath, (++))
import           Shelly as Shelly
import           Text.Printf
import qualified Text.Regex.PCRE.Heavy as Re
import qualified Text.Regex.PCRE.Light as Re
import Data.String (fromString)
import Data.Maybe


default (T.Text)


(++) :: Monoid a => a -> a -> a
(++) = mappend


graphFilesRegex :: Re.Regex
graphFilesRegex = Re.compile "(run_test_level(\\d+)_(\\d+)).hs$" []



graphGenerationBinaryLocation :: FilePath
graphGenerationBinaryLocation = "../rand-code-graph/.stack-work/install/x86_64-osx/lts-6.4/7.10.3/bin/random-level-graphs"


outputLocation :: FilePath
outputLocation = "results"


graphsToGenerate :: Int
graphsToGenerate = 4


maxLevel :: Int
maxLevel = 9


seed :: T.Text
seed = "12345"


runOne :: T.Text -> Sh ()
runOne type_ = do
    echo $ T.pack $ printf "Building and testing %i graphs" (graphsToGenerate * maxLevel)
    let genPath = "generated"
    ls genPath >>= mapM_ (\x -> echo ("removing " <> T.pack (show x)) >> rm x) . filter (not . (`elem` [".", ".."]))
    (generationTime, _) <- time $ run
        graphGenerationBinaryLocation
        [ "-L", type_
        , "-o", "generated/"
        , "-l", T.pack $ show maxLevel
        , "-n", T.pack $ show (graphsToGenerate * maxLevel)
        , "-s", seed
        ]
    files <- fmap catMaybes $ (filter (not . (`elem` [".", ".."])) <$> lsT genPath) >>= mapM (\filepath ->
        case Re.scan graphFilesRegex filepath of
            [(_, [funname, level, index])] -> do
                content <- readfile $ fromString $ T.unpack filepath
                return $ Just $ (content, (funname, level, index :: T.Text))
            _ -> return Nothing
        )
    let (code, functions) = unzip files
    pream <- readfile "resources/Preamble.hs"
    let allTests :: T.Text
        allTests =
               "allTests :: [(Env u -> IO Int, Int, Int)]\n"
            <> "allTests = [(" ++ T.intercalate "), (" (map (\(name, level, index) -> T.intercalate "," [name, level, index]) functions) ++ ")]"
    let fullContent = T.intercalate "\n" $ pream:allTests:code :: T.Text
    writefile (genPath </> "TestGraphs.hs") fullContent

    echo $ T.pack $ printf "Generated %i graphs in %s" (graphsToGenerate * maxLevel) (formatSeconds $ ceiling generationTime)
    (compilationTime, _) <- time $ run "cabal" ["build", "haxl-test-generated-graph-exe"]
    echo $ T.pack $ printf "Compiled test program in %s" (formatSeconds $ ceiling compilationTime)
    (runTime, _) <- time $ run "./dist/build/haxl-test-generated-graph-exe/haxl-test-generated-graph-exe" [] >>=
        writefile (outputLocation </> fromString ("haskell-" ++ T.unpack type_) <.>"json")
    echo $ T.pack $ printf "Ran program in %s" (formatSeconds $ ceiling runTime)



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

    runOne "HaskellDo"
    runOne "HaskellDoApp"
