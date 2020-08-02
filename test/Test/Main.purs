module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console as Console
import Test.Code as Code
import Test.Common (yellow)
import Test.Json as Json
import Test.Prose as Prose

main :: Effect Unit
main = do
  Console.log "" -- newline
  runTests "Prose tests" Prose.test
  runTests "Code tests" Code.test
  runTests "Json tests" Json.test

runTests :: String -> Effect Unit -> Effect Unit
runTests name test = do
  Console.log $ yellow (name <> "\n")
  test
