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


data ExpType = If | Func deriving Show


before :: T.Text
before = "{-# LANGUAGE NoImplicitPrelude #-}\n\
          \{-# LANGUAGE RebindableSyntax  #-}"
after :: T.Text
after = "import           Haxl.Core\n\
         \import           Haxl.Prelude\n\
         \import           Lib\n\
         \import           Prelude      hiding ((>>))\n"


runOneFunc :: T.Text -> Sh ()
runOneFunc style = do
    echo $ T.pack $ printf "Building and testing %i graphs" (graphsToGenerate * maxLevel)
    let genPath = "generated"
    ls genPath >>= mapM_ (\x -> echo ("removing " <> T.pack (show x)) >> rm x) . filter (not . (`elem` [".", ".."]))
    (generationTime, _) <- time $ run
        graphGenerationBinaryLocation
        [ "-L", style
        , "-o", "generated/"
        , "-l", T.pack $ show maxLevel
        , "-n", T.pack $ show (graphsToGenerate * maxLevel)
        , "-s", seed
        , "--percentageifs", "0.3"
        , "--percentagefuns",  "0.3"
        ]
    pream <- readfile "resources/Preamble.hs"
    functions <- fmap catMaybes $ (filter (not . (`elem` [".", ".."])) <$> lsT genPath) >>= mapM (\filepath ->
        case Re.scan graphFilesRegex filepath of
            [(_, [funname, level, index])] -> do
                content <- readfile $ fromString $ T.unpack filepath
                let modname = "M" ++ funname
                let fullCode = T.intercalate "\n" [before, "module " ++ modname ++ " where\n", after, content]
                writefile (fromString $ T.unpack $ "generated/" ++ modname ++ ".hs") fullCode
                return $ Just $ (modname, funname, level, index :: T.Text)
            _ -> return Nothing
        )
    let imports = T.intercalate "\n" $ map (\(modname, _, _, _) -> "import qualified " ++ modname) functions
    let allTests :: T.Text
        allTests =
               "allTests :: [(Env () -> IO Int, Int, Int)]\n"
            <> "allTests = [(" ++ T.intercalate "), (" (map (\(modname, name, level, index) -> T.intercalate "," [modname ++ "." ++ name, level, index]) functions) ++ ")]"
    let fullContent = T.intercalate "\n" [pream, imports, allTests]
    writefile (genPath </> "TestGraphs.hs") fullContent


    echo $ T.pack $ printf "Generated %i graphs in %s" (graphsToGenerate * maxLevel) (formatSeconds $ ceiling generationTime)
    (compilationTime, _) <- time $ run "cabal" ["build", "haxl-test-generated-graph-exe"]
    echo $ T.pack $ printf "Compiled test program in %s" (formatSeconds $ ceiling compilationTime)
    (runTime, _) <- time $ run "./dist/build/haxl-test-generated-graph-exe/haxl-test-generated-graph-exe" [] >>=
        writefile (outputLocation </> fromString ("haskell-func-" ++ T.unpack style) <.>"json")
    echo $ T.pack $ printf "Ran program in %s" (formatSeconds $ ceiling runTime)


runOne :: T.Text -> Sh ()
runOne style = do
    echo $ T.pack $ printf "Building and testing %i graphs" (graphsToGenerate * maxLevel)
    let genPath = "generated"
    ls genPath >>= mapM_ (\x -> echo ("removing " <> T.pack (show x)) >> rm x) . filter (not . (`elem` [".", ".."]))
    (generationTime, _) <- time $ run
        graphGenerationBinaryLocation
        [ "-L", style
        , "-o", "generated/"
        , "-l", T.pack $ show maxLevel
        , "-n", T.pack $ show (graphsToGenerate * maxLevel)
        , "-s", seed
        ,"--percentageifs", "1"
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
        writefile (outputLocation </> fromString ("haskell-if-" ++ T.unpack style) <.>"json")
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

    -- runOne "HaxlDo"
    -- runOne "HaxlDoApp"
    runOneFunc "HaxlDo"
    runOneFunc "HaxlDoApp"
