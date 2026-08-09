module Test.Main where
import Prelude
import Effect (Effect)
import Test.Data.Array (testArray)
main :: Effect Unit
main = testArray
