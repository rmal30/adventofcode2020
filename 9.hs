import qualified Data.Set as S
import Data.List(scanl')

getPairsWithSum :: Integral a => S.Set a -> a -> [(a, a)]
getPairsWithSum set target = [(element1, element2) | element1 <- S.elems set, let element2 = target - element1, S.member element2 set]

series :: Num a => [a] -> [a]
series = scanl' (+) 0

main :: IO ()
main = do
    contents <- readFile "inputs/9.txt"
    let numbers = map read (lines contents) :: [Int]
    let partialSums = series numbers
    let preambleSize = 25
    let (_, part1):_ = dropWhile (\(i, v) -> not (null (getPairsWithSum (S.fromList (take preambleSize (drop (i - preambleSize) numbers))) v))) (drop preambleSize (zip [0..] numbers))
    let (end, start):_ = [(i, j) | (i, a) <- zip [0..] partialSums, (j, b) <- take i (zip [0..] partialSums), a - b == part1]
    let conseqItems = drop start (take end numbers)
    let part2 = minimum conseqItems + maximum conseqItems
    print (part1, part2)
