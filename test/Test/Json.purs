module Test.Json (test) where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Test.Common (runTest)
import Text.Pretty (Doc, enclose, flatAltFn, group, indent, line, punctuate, sep, text, (<+>))
import Text.Pretty.String (comma, lbracket, rbracket, lbrace, rbrace, dquotes)


test :: Effect Unit
test = do
    runTest 50 (prettyJson exampleArray)
    runTest 10 (prettyJson exampleArray)
    runTest 50 (prettyJson smallObject)
    runTest 10 (prettyJson smallObject)
    runTest 30 (prettyJson nestedObject)


exampleArray :: Json
exampleArray = JArray
    [ JNumber 2.0
    , JNumber 4.0
    , JString "foo"
    , JNull
    ]


smallObject :: Json
smallObject = JObject
    [ Tuple "foo" JNull
    , Tuple "bar" JNull
    ]


nestedObject :: Json
nestedObject = JObject
    [ Tuple "foo" smallObject
    , Tuple "bar" smallObject
    ]


data Json
    = JObject Object
    | JArray (Array Json)
    | JString String
    | JNumber Number
    | JBool Boolean
    | JNull


type Object = Array (Tuple String Json)


prettyJson :: Json -> Doc String
prettyJson JNull       = text "null"
prettyJson (JBool b)   = if b then text "true" else text "false"
prettyJson (JNumber n) = text (show n)
prettyJson (JString s) = text s
prettyJson (JArray vs) = group (prettyJsonArray vs)
prettyJson (JObject o) = group (prettyJsonObject o)


prettyJsonArray :: Array Json -> Doc String
prettyJsonArray [] = text "[]"
prettyJsonArray vs =
    map prettyJson vs #
    punctuate comma >>>
    sep >>>
    flatAltFn (indent 2) identity >>>
    brackets


prettyJsonObject :: Object -> Doc String
prettyJsonObject [] = text "{}"
prettyJsonObject o  =
    map keyValue o #
    punctuate comma >>>
    sep >>>
    flatAltFn (indent 2) identity >>>
    braces
  where
    keyValue :: Tuple String Json -> Doc String
    keyValue (Tuple k v) =
        dquotes (text k) <+> prettyJson v


brackets :: Doc String -> Doc String
brackets = enclose (lbracket <> line) (line <> rbracket)


braces :: Doc String -> Doc String
braces = enclose (lbrace <> line) (line <> rbrace)
