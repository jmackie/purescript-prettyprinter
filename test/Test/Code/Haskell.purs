module Test.Code.Haskell (test) where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (intercalate)
import Data.NonEmpty ((:|))
import Data.Renderable (class Renderable)
import Data.String as String
import Effect (Effect)
import Test.Common (runTest, green)
import Text.Pretty ((<+>))
import Text.Pretty as Pretty

test :: Effect Unit
test = do
  let docList =
        Pretty.list
        [ Pretty.text "1"
        , Pretty.text "2"
        , Pretty.text "3"
        ]
  runTest 10 docList
  runTest 5 docList
