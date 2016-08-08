{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import           Data.Foldable         (for_)
import           Data.List             (stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.String           (fromString)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Traversable      (for)
import           Debug.Trace
import           Experiment.Haxl.Types
import           GHC.Generics
import           Prelude               hiding (FilePath, (++))
import           Shelly                as Shelly
import           System.Environment    (getArgs)
import           Text.Printf
import qualified Text.Regex.PCRE.Heavy as Re
import qualified Text.Regex.PCRE.Light as Re


default (T.Text)

(++) :: Monoid a => a -> a -> a
(++) = mappend

graphFilesRegex :: Re.Regex
graphFilesRegex = Re.compile "(run_test_level(\\d+)_(\\d+)).hs$" []

graphGenerationBinaryLocation :: FilePath
graphGenerationBinaryLocation = "random-level-graphs"

outputLocation :: FilePath
outputLocation = "results"


baseConf = MkGenConf
    { numLevels = 7
    , numGraphs = 7*5
    , seed = Just 123456
    , prctIfs = Nothing
    , prctFuns = Nothing
    , prctMaps = Nothing
    , slowDataSource = Nothing
    }


before :: T.Text
before = "{-# LANGUAGE NoImplicitPrelude #-}\n\
          \{-# LANGUAGE RebindableSyntax  #-}"
after :: T.Text
after = "import           Haxl.Core\n\
         \import           Haxl.Prelude\n\
         \import           Lib\n\
         \import           Prelude      hiding ((>>), mapM)\n"


mParam :: Show a => T.Text -> Maybe a -> [T.Text]
mParam name = maybe [] (\v -> [name, showt v])


showt :: Show a => a -> T.Text
showt = T.pack . show


runOneFunc :: FilePath -> [GenConf] -> Sh ()
runOneFunc expName toGen = do
    -- echo $ T.pack $ printf "Building and testing %i graphs" (graphsToGenerate * maxLevel)
    let genPath = "generated"
    ls genPath >>= mapM_ rm . filter (not . (`elem` [".", ".."]))
    pream <- readfile "../resources/Preamble.hs"
    (confs, functions) <- unzip . catMaybes . concat <$> for (zip toGen [1..]) (\(conf, globalIndex) -> do
        run
            graphGenerationBinaryLocation
            $ [ "-L", lang conf
              , "-o", "generated/"
              , "-l", showt $ numLevels conf
              , "-n", showt $ numGraphs conf
              ]
              ++ mParam "-s" (seed conf)
              ++ mParam "--percentagefuns" (prctFuns conf)
              ++ mParam "--percentagemaps" (prctMaps conf)
              ++ mParam "--percentageif" (prctIfs conf)
              ++ case slowDataSource conf of
                    Just True -> ["-S"]
                    _ -> []
        generated <- filter (not . (`elem` [".", ".."])) <$> lsT genPath
        for generated $ \mname ->
            case Re.scan graphFilesRegex mname of
                [(_, [funname, level, index])] -> do
                    content <- readfile $ fromString $ T.unpack mname
                    let modname = "M" ++ funname ++ "_" ++ T.pack (show (globalIndex :: Int))
                    let fullCode = T.intercalate "\n" [before, "module " ++ modname ++ " where\n", after, content]
                    writefile (fromString $ T.unpack $ "generated/" ++ modname ++ ".hs") fullCode
                    return $ Just $ (conf, ) $ (modname, funname, level, index :: T.Text)
                _ -> return Nothing
        )
    let imports = T.intercalate "\n" $ map (\(modname, _, _, _) -> "import qualified " ++ modname) functions
    let allTests :: T.Text
        allTests =
            "allTests :: [(Env () -> IO Int, Int, Int)]\n"
            <> "allTests = [(" ++ T.intercalate "), (" (map (\(modname, name, level, index) -> T.intercalate "," [modname ++ "." ++ name, level, index]) functions) ++ ")]"
    let fullContent = T.intercalate "\n" [pream, imports, allTests]
    writefile (genPath </> "TestGraphs.hs") fullContent

    -- echo $ T.pack $ printf "Generated %i graphs in %s" (graphsToGenerate * maxLevel) (formatSeconds $ ceiling generationTime)
    (compilationTime, _) <- time $ run "stack" ["build"]
    echo $ T.pack $ printf "Compiled test program in %s" (formatSeconds $ ceiling compilationTime)
    rawData <- run "stack" ["exec", "inner-app-exe"]

    let jsonData = fromMaybe (error "json unreadable") $ decode $ B.fromStrict $ encodeUtf8 rawData

    let withPercent = map (\(mg, conf) -> mg { genConf = Just conf } ) (zip jsonData confs)

    writefile (outputLocation </> expName <.> "json") (decodeUtf8 $ B.toStrict $ encode withPercent)
    -- echo $ T.pack $ printf "Ran program in %s" (formatSeconds $ ceiling runTime)


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
main = do
    [type_] <- getArgs
    let if_conf = [ baseConf {numLevels=20, numGraphs=1, lang="HaxlDoApp", seed=Just myseed, prctIfs=Just percentage}
                  | myseed <- [123456, 234567]
                  , percentage <- [0.1, 0.2, 0.3, 0.4]
                  ]
        delayed = map (\c -> c { slowDataSource = Just True}) if_conf
        !action = case type_ of
                    "if" -> runOneFunc "haskell-if" if_conf
                    "if-delayed" -> runOneFunc "haskell-if-delayed" delayed
                    "map" -> runOneFunc "haskell-map" [ baseConf {numLevels=20, numGraphs=1, lang="HaxlDoApp", seed=Just myseed, prctMaps=Just percentage}
                                                      | myseed <- [123456, 234567]
                                                      , percentage <- [0.1, 0.2, 0.3, 0.4]
                                                      ]
                    "func" -> runOneFunc "haskell-func" [ baseConf {numLevels=20, numGraphs=1, lang="HaxlDoApp", seed=Just myseed, prctFuns=Just percentage}
                                                        | myseed <- [123456, 234567]
                                                        , percentage <- [0.1, 0.2, 0.3, 0.4]
                                                        ]

    shelly $ print_stdout False $ escaping False $ do
        cd "inner-app"
        mkdir_p outputLocation
        mkdir_p "generated"

        run "cabal" ["install", "--only-dependencies"]

        action
