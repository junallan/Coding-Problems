module Main where

import Data.Maybe

main :: IO ()
main = mapM_ (putStrLn . (\(minRange, maxRange) -> "(" ++ show minRange ++ ", " ++ show maxRange ++ ")")) (longestContiguousIncreasingRange [1, 3, 5, -9, 11, 12, 13, 15, 78484348, 11, 12 ,13, 0, -1, 343, 1222, 0])

longestContiguousIncreasingRange :: [Int] -> [(Int,Int)]
longestContiguousIncreasingRange items = maxPositionRange rangedItems
                                         where
                                             rangedItems = (positionsRanges . positionsDecreaseInItem) items

positionsDecreaseInItem :: [Int] -> [Int]
positionsDecreaseInItem items = map fromJust (filter isJust (map (\i -> if i == length items - 1 || items !! i > items !! (i+1) then Just i else Nothing) [0 .. length items-1]))

positionsRanges :: [Int] -> [(Int,Int)]
positionsRanges [] = []
positionsRanges [item] = [(0,item)]
positionsRanges items = (0, head items) : map (\i -> (items !! (i - 1) + 1,items !! i)) [1.. length items - 1]

maxPositionRange :: [(Int,Int)] -> [(Int,Int)]
maxPositionRange [] = []
maxPositionRange [(minVal, maxVal)] = [(minVal, maxVal)]
maxPositionRange items = filter (\item -> rangeLength item == maxLength) items
                            where
                                maxLength = maximum $ map rangeLength items 

rangeLength :: (Int,Int) -> Int
rangeLength (minVal, maxVal) = maxVal - minVal  