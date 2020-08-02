module Test.Code.Haskell (test) where

import Prelude
import Effect (Effect)
import Test.Common (runTest)
import Text.Pretty (text) as Pretty
import Text.Pretty.Code.Haskell (list) as Pretty

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
