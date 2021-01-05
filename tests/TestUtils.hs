{-# LANGUAGE TemplateHaskell #-}

module TestUtils
    ( allTests
    ) where

import qualified Data.List as List
import qualified Utils
import Test.QuickCheck
import Test.QuickCheck.All


-- Length of resulting list is equal to number of unique elements.
prop_groupBy_1 :: (Eq a) => [a] -> Bool
prop_groupBy_1 xs = unique xs == length (Utils.groupBy (==) xs)

-- Resulting list should not contain empty lists.
prop_groupBy_2 :: (Eq a) => [a] -> Bool
prop_groupBy_2 xs = all (not . null) $ Utils.groupBy (==) xs

-- Length of each sublist from result is equal to a number of elements
-- that sublist is made of.
prop_groupBy_3 :: (Eq a) => [a] -> Bool
prop_groupBy_3 xs = all true $ checkSublist <$> Utils.groupBy (==) xs
    where checkSublist sublist = length sublist == uniqueElement (head sublist) xs

-- Each sublist of result consists of one unique element.
prop_groupBy_4 :: (Eq a) => [a] -> Bool
prop_groupBy_4 xs = all (\x -> unique x == 1) (Utils.groupBy (==) xs)

-- If list is not empty -- return sum of it's elements.
prop_sumBy_1 :: (Num a, Eq a) => [a] -> Property
prop_sumBy_1 xs = (not . null) xs ==> List.sum xs == Utils.sumBy (+) undefined xs

-- If list is empty -- return default value as a result.
prop_sumBy_2 :: (Num a, Eq a) => a -> Bool
prop_sumBy_2 a = Utils.sumBy (+) a [] == a


-- Helpers
unique :: (Eq a) => [a] -> Int
unique = length . List.nub

uniqueElement :: (Eq a) => a -> [a] -> Int
uniqueElement x = length . List.filter (x ==)

true :: Bool -> Bool
true = id

-- Hack to run all tests
return []
allTests :: [(String, Property)]
allTests = $(allProperties)
