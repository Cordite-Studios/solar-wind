module Test where

-- Test imports
import Test.Tasty

import Time
import Delta

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [time, delta]
