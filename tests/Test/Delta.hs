{-# LANGUAGE ScopedTypeVariables #-}
module Test.Delta where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Solar.Utility.Delta

delta :: TestTree
delta = testGroup "Solar.Utility.Delta"
    [ QC.testProperty "Sum is always >= 0" $
        \(v::[Positive Integer]) ->
            let x = map (DeltaContainer ()) v
            in 0 <= sumQueue x
    , QC.testProperty "Zipping around keeps queue same size" $
        \(v::[Positive Integer]) (skip::Positive Integer) ->
            let x = map (DeltaContainer ()) v
                (z1, z2) = zipTo x skip
            in length v == length z1 + length z2
    , QC.testProperty "Zipping around never increases counts" $
        \(v::[Positive Integer]) (skip::Positive Integer) ->
            let x = map (DeltaContainer ()) v
                (z1, z2) = zipTo x skip
            in sumQueue x >= sumQueue z1 + sumQueue z2
    , QC.testProperty "Zipping over always has a 0 sum queue" $
        \(v::[Positive Integer]) (skip::Positive Integer) ->
            let x = map (DeltaContainer ()) v
                (z1, _) = zipOver x skip
            in sumQueue z1 == Positive 0
    , QC.testProperty "Insertion makes a bigger queue" $ 
        \(v::[Positive Integer]) v' ->
            let dc = DeltaContainer ()
                x = map dc v
                x' = foldl (\a val -> insert (dc val) a) x v'
            in if null v'
                then length x' == length x
                else length x' > length x
    ]