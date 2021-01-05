module Utils
    ( groupBy
    , sumBy
    ) where

import Data.List (foldl')

-- | 'groupBy' is like Data.List.groupBy function but it
--   differs in a way that its result consist of unique sublists each
--   contains all occurrences of the same item.
-- >> groupBy (==) [1, 2, 3, 1, 2, 3, 1, 2, 3]
-- >> [[1, 1, 1], [2, 2, 2], [3, 3, 3]]
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq = foldl' (addOrCreateToSublist eq) []

-- | 'sumBy' sum elements of list using summator function
--   with first element of list as initial value, but if
--   list is empty -- return custom initial value.
sumBy :: (a -> a -> a) -> a -> [a] -> a
sumBy _ a [] = a
sumBy _ _ [x] = x
sumBy plus _ (x:y:[]) = x `plus` y
sumBy plus _ (x:y:xs) = x `plus` y `plus` sumBy plus undefined xs

addOrCreateToSublist :: (a -> a -> Bool) -> [[a]] -> a -> [[a]]
addOrCreateToSublist _ [] a = [[a]]
addOrCreateToSublist eq (sublist@(x:_):ys) a
    | x `eq` a = (sublist ++ [a]) : ys
    | otherwise = sublist : addOrCreateToSublist eq ys a
