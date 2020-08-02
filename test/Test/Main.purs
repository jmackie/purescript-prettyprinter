module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console as Console
import Test.Code.FunctionSignature as Code.FunctionSignature
import Test.Code.Haskell as Code.Haskell
import Test.Code.Purescript as Code.Purescript
import Test.Common (yellow)
import Test.Json as Json
import Test.Prose as Prose

main :: Effect Unit
main = do
  Console.log "" -- newline
  runTests "Prose tests" Prose.test
  runTests "Code FunctionSignature tests" Code.FunctionSignature.test
  runTests "Code Haskell tests" Code.Haskell.test
  runTests "Code Purescript tests" Code.Purescript.test
  runTests "Json tests" Json.test

runTests :: String -> Effect Unit -> Effect Unit
runTests name test = do
  Console.log $ yellow (name <> "\n")
  test
