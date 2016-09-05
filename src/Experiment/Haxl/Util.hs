{-# LANGUAGE LambdaCase #-}
module Experiment.Haxl.Util where


import           ClassyPrelude
import           Data.Aeson.Types


rewritePrefixes :: [(String, String)] -> (String -> String) -> String -> String
rewritePrefixes [] inner source = inner source
rewritePrefixes ((trigger, target):xs) inner source =
    maybe (rewritePrefixes xs inner source) ((target ++) . inner) $ stripPrefix trigger source


gconfPrefixTrans :: [(String, String)]
gconfPrefixTrans =
    [ ("prct", "%")
    , ("num", "#")
    ]



gconfOptions = defaultOptions {fieldLabelModifier = \case
                                                        "slowDataSource" -> "+slow"
                                                        a -> rewritePrefixes gconfPrefixTrans (camelTo2 '_') a
                              , omitNothingFields = True}
