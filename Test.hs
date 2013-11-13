module Test where

-- Test imports
import Test.Tasty

import Test.Time
import Test.Delta

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [time, delta]
