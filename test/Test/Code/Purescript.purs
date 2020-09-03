module Test.Code.Purescript (test) where

import Prelude
import Effect (Effect)
import Test.Common (runTest)
import PrettyprinterRenderable (text) as Pretty
import PrettyprinterRenderable.Code.Purescript (list) as Pretty

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

  let docListNested =
        Pretty.list
        [ Pretty.text "1"
        , Pretty.text "2"
        , Pretty.text "3"
        , Pretty.list
          [ Pretty.text "1"
          , Pretty.text "2"
          , Pretty.text "3"
          ]
        ]
  runTest 30 docListNested
  runTest 5 docListNested
