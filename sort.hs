import Data.List

sortBySubLength :: [[a]] -> [[a]]
sortBySubLength = sortBy compare
    where
        compare xs ys
            | (length xs) < (length ys) = GT
            | otherwise = LT
