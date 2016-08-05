#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveGeneric, TupleSections        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings, BangPatterns    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module RunTests where


import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy  as B
import           Data.Foldable         (for_)
import           Data.Maybe
import           Data.Monoid
import           Data.String           (fromString)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Traversable      (for)
import           Debug.Trace
import           GHC.Generics
import           Prelude               hiding (FilePath, (++))
import           Shelly                as Shelly
import           Text.Printf
import qualified Text.Regex.PCRE.Heavy as Re
import qualified Text.Regex.PCRE.Light as Re
import Data.List (stripPrefix)
import System.Environment (getArgs)


default (T.Text)

(++) :: Monoid a => a -> a -> a
(++) = mappend

graphFilesRegex :: Re.Regex
graphFilesRegex = Re.compile "(run_test_level(\\d+)_(\\d+)).hs$" []

graphGenerationBinaryLocation :: FilePath
graphGenerationBinaryLocation = "../rand-code-graph/.stack-work/install/x86_64-osx/lts-6.4/7.10.3/bin/random-level-graphs"

outputLocation :: FilePath
outputLocation = "results"


data MeasuredGraph = MeasuredGraph
    { nr               :: Maybe Int
    , levels           :: Int
    , rounds       :: Int
    , fetches :: Int
    , genConf          :: Maybe GenConf
    } deriving (Generic, Show, Eq, Ord)


type MeasuredGraphs = [MeasuredGraph]


instance ToJSON MeasuredGraph where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelTo '_' }

instance FromJSON MeasuredGraph where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelTo '_' }


data GenConf = MkGenConf
    { lang      :: T.Text
    , numLevels :: Int
    , numGraphs :: Int
    , seed      :: Maybe Int
    , prctFuns  :: Maybe Float
    , prctMaps  :: Maybe Float
    , prctIf    :: Maybe Float
    } deriving (Generic, Show, Eq, Ord)


baseConf = MkGenConf
    { numLevels = 7
    , numGraphs = 7*5
    , seed = Just 123456
    , prctIf = Nothing
    , prctFuns = Nothing
    , prctMaps = Nothing
    }


rewritePrefixes :: [(String, String)] -> (String -> String) -> String -> String
rewritePrefixes [] inner source = inner source
rewritePrefixes ((trigger, target):xs) inner source =
    maybe (rewritePrefixes xs inner source) ((target ++) . inner) $ stripPrefix trigger source


gconfPrefixTrans :: [(String, String)]
gconfPrefixTrans =
    [ ("prct", "%")
    , ("num", "#")
    ]

gconfOptions = defaultOptions {fieldLabelModifier= rewritePrefixes gconfPrefixTrans (camelTo '_')}

instance ToJSON GenConf where
    toJSON = genericToJSON gconfOptions

instance FromJSON GenConf where
    parseJSON = genericParseJSON gconfOptions


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
    pream <- readfile "resources/Preamble.hs"
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
              ++ mParam "--percentageif" (prctIf conf)
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
    (compilationTime, _) <- time $ run "cabal" ["build", "haxl-test-generated-graph-exe"]
    echo $ T.pack $ printf "Compiled test program in %s" (formatSeconds $ ceiling compilationTime)
    rawData <- run "./dist/build/haxl-test-generated-graph-exe/haxl-test-generated-graph-exe" []

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

    let !action = case type_ of
                    "map" -> runOneFunc "haskell-map" [baseConf {lang="HaxlDoApp", prctMaps=Just 0.4}]
                    "func" -> runOneFunc "haskell-func" [ baseConf {numLevels=20, numGraphs=1, lang="HaxlDoApp", seed=Just myseed, prctFuns=Just percentage}
                                                        | myseed <- [123456, 234567]
                                                        , percentage <- [0.1, 0.2, 0.3, 0.4]
                                                        ]

    shelly $ print_stdout False $ escaping False $ do
        mkdir_p outputLocation
        mkdir_p "generated"

        run "cabal" ["install", "--only-dependencies"]

        action
