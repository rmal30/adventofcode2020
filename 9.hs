import qualified Data.IntSet as S
import Data.List(scanl')

getPairsWithSumFast :: [Int] -> Int -> [(Int, Int)]
getPairsWithSumFast values target = filter (\(_, v2) -> S.member v2 valueSet) [(v, target - v) | v <- values]
    where
        valueSet = S.fromList values

series = scanl' (+) 0

main = do
    contents <- readFile "inputs/9.txt"
    let numbers = map read (lines contents)
    let partialSums = series numbers
    let preambleSize = 25
    let (_, part1) = (head (dropWhile (\(i, v) -> not (null (getPairsWithSumFast (take preambleSize (drop (i - preambleSize) numbers)) v))) (zip [preambleSize..(length numbers - 1)] (drop preambleSize numbers))))
    let (i, j, a, b) = head (filter (\(_, _, a, b) -> a - b == part1) [(i, j, a, b) | (i, a) <- zip [0..] partialSums, (j, b) <- takeWhile(\(j, _) -> j < i) (zip [0..] partialSums)])
    let conseqItems = drop j (take i numbers)
    let part2 = minimum conseqItems + maximum conseqItems
    print (part1, part2)
