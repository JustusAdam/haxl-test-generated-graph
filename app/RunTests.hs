{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


import           ClassyPrelude         hiding (FilePath, print, (<.>), (</>))
import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import           Data.Text.Format
import           Experiment.Haxl.Types hiding (time)
import           Shelly
import qualified Text.Regex.PCRE.Heavy as Re
import qualified Text.Regex.PCRE.Light as Re


default (Text)

graphFilesRegex :: Re.Regex
graphFilesRegex = Re.compile "(run_test_level(\\d+)_(\\d+)).hs$" []

graphGenerationBinaryLocation :: FilePath
graphGenerationBinaryLocation = "random-level-graphs"

outputLocation :: FilePath
outputLocation = "../results"


baseConf = MkGenConf
    { numLevels = 7
    , numGraphs = 7*5
    , seed = Just 123456
    , prctIfs = Nothing
    , prctFuns = Nothing
    , prctMaps = Nothing
    , prctSlow = Nothing
    , slowDataSource = Nothing
    , inlineIf = Nothing
    , lang="HaxlDoApp"
    }


tmplate :: Format
tmplate = "{-# LANGUAGE NoImplicitPrelude #-}\n\
          \{-# LANGUAGE RebindableSyntax  #-}\n\
          \module {} where\n\
          \\n\
          \import           Haxl.Core\n\
          \import           Haxl.Prelude\n\
          \import           Lib\n\
          \import           Prelude      hiding ((>>), mapM)\n\
          \\n\
          \\n\
          \{}\n"
slowTmplate :: Format
slowTmplate = "{-# LANGUAGE NoImplicitPrelude #-}\n\
              \{-# LANGUAGE RebindableSyntax  #-}\n\
              \module {} where\n\
              \\n\
              \import           Haxl.Core\n\
              \import           Haxl.Prelude\n\
              \import           SlowLib\n\
              \import           Prelude      hiding ((>>), mapM)\n\
              \\n\
              \\n\
              \{}\n"

mParam :: Show a => Text -> Maybe a -> [Text]
mParam name = maybe [] (\v -> [name, showt v])


showt :: Show a => a -> Text
showt = pack . show


runOneFunc :: FilePath -> [GenConf] -> Sh ()
runOneFunc expName toGen = do
    print "Building and testing {} graphs\n" [sum $ map numGraphs toGen]
    let genPath = "generated"
    ls genPath >>= mapM_ rm . filter (not . (`elem` [".", ".."]))
    pream <- readfile "../resources/Preamble.hs"
    (confs, functions) <- unzip . catMaybes . concat <$> for (zip toGen [1..]) (\(conf, globalIndex) -> do
        let useSlow = case slowDataSource conf of
                        Just True -> True
                        _ -> False
        void $ run
            graphGenerationBinaryLocation
            $ [ "-L", lang conf
              , "-o", "generated/"
              , "-l", showt $ numLevels conf
              , "-n", showt $ numGraphs conf
              ]
              ++ mParam "-s" (seed conf)
              ++ mParam "--percentageslow" (prctSlow conf)
              ++ mParam "--percentagefuns" (prctFuns conf)
              ++ mParam "--percentagemaps" (prctMaps conf)
              ++ mParam "--percentageif" (prctIfs conf)
              ++ case slowDataSource conf of
                    Just True -> ["-S"]
                    _ -> []
              ++ case inlineIf conf of
                    Just True -> ["-i"]
                    _ -> []
        generated <- filter (not . (`elem` [".", ".."])) <$> lsT genPath
        for generated $ \mname ->
            case Re.scan graphFilesRegex mname of
                [(_, [funname, level, index])] -> do
                    content <- readfile $ fromString $ unpack mname
                    let modname = "M" ++ funname ++ "_" ++ pack (show (globalIndex :: Int))
                    let fullCode = format (if useSlow then slowTmplate else tmplate) (modname, content)
                    writefile (fromString $ unpack $ "generated/" ++ modname ++ ".hs") (toStrict fullCode)
                    return $ Just (conf, (modname, funname, level, index :: Text))
                _ -> return Nothing
        )
    let imports = intercalate "\n" $ map (\(modname, _, _, _) -> "import qualified " ++ modname) functions
    let allTests = toStrict $ format
            "allTests :: [(Env () -> IO Int, Int, Int)]\n\
            \allTests = [({})]" [intercalate "), (" (map (\(modname, name, level, index) -> intercalate "," [modname ++ "." ++ name, level, index]) functions)]
    let fullContent = intercalate "\n" [pream, imports, allTests]
    writefile (genPath </> "TestGraphs.hs") fullContent

    -- echo $ T.pack $ printf "Generated %i graphs in %s" (graphsToGenerate * maxLevel) (formatSeconds $ ceiling generationTime)
    (compilationTime, _) <- time $ run "stack" ["build"]
    print "Compiled test program in {}\n" [formatSeconds $ ceiling compilationTime]
    (runTime, rawData) <- time $ run "stack" ["exec", "inner-app-exe"]

    let jsonData = fromMaybe (error "json unreadable") $ decode $ B.fromStrict $ encodeUtf8 rawData

    let withPercent = map (\(mg, conf) -> mg { genConf = Just conf } ) (zip jsonData confs)

    print "Writing results to {}\n" [show $ outputLocation </> expName <.> "json"]

    writefile (outputLocation </> expName <.> "json") (decodeUtf8 $ B.toStrict $ encode withPercent)
    print "Ran program in {}\n" [formatSeconds $ ceiling runTime]


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

basePrctConf :: GenConf
basePrctConf = baseConf { numGraphs = 1, numLevels = 10 }

if_conf :: [GenConf]
if_conf = [ basePrctConf {lang="HaxlDoApp", seed=Just myseed, prctIfs=Just percentage, numLevels=30, inlineIf=Just inline}
          | myseed <- [123456, 234567]
          , percentage <- [0.7, 0.8, 0.9, 1]
          , inline <- [True, False]
          ]
delayed :: [GenConf]
delayed = map (\c -> c { slowDataSource = Just True}) if_conf

mapConfs :: [GenConf]
mapConfs = [ basePrctConf { lang="HaxlDoApp", seed=Just myseed, prctMaps=Just percentage, numLevels=10}
           | myseed <- [123456, 234567]
           , percentage <- [0.1, 0.15, 0.2, 0.25, 0.275, 0.3, 0.325, 0.375, 0.35]
           ]


experiments :: [(Text, Sh ())]
experiments =
    [ ("if", runOneFunc "haskell-if" if_conf)
    , ("if-delayed", runOneFunc "haskell-if-delayed" delayed)
    , ("map", runOneFunc "haskell-map" mapConfs)
    , ("func",  runOneFunc "haskell-func" [ basePrctConf { lang="HaxlDoApp", seed=Just myseed, prctFuns=Just percentage}
                                          | myseed <- [123456, 234567]
                                          , percentage <- [0.1, 0.2, 0.3, 0.4]
                                          ])
    , ("temp", runOneFunc "temp" $ return basePrctConf { numLevels = 5, prctMaps = Just 0.6, seed = Just 123456 } )
    , ("vanilla", runOneFunc "haskell-vanilla" $ return baseConf { numLevels = 20, numGraphs = 100, seed = Just 123456 })
    , ("map-primer", runOneFunc "haskell-map-primer" (map (\cfg@(MkGenConf{prctMaps=prct}) -> cfg {prctMaps = Nothing, prctFuns=prct}) mapConfs))
    , ("all", mapM_ snd (filter ((/= "all") . fst) experiments))
    ]


main :: IO ()
main = do
    types <- getArgs

    shelly $ print_stdout False $ escaping False $ do
        cd "inner-app"
        mkdir_p outputLocation
        mkdir_p "generated"

        void $ run "stack" ["install", "--only-dependencies"]

        for_ types $ \exp ->
            case lookup exp experiments of
                Nothing -> echo $ "No experiment " ++ exp ++ " defined"
                Just action -> action
