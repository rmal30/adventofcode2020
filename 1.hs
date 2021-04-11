import qualified Data.IntSet as S

getPairsWithSum :: [Int] -> Int -> [(Int, Int)]
getPairsWithSum values target = [(v1, v2) | v1 <- values, v2 <- values, v1 + v2 == target, v1 /= v2]

getPairsWithSumFast :: [Int] -> Int -> [(Int, Int)]
getPairsWithSumFast values target = filter (\(_, v2) -> S.member v2 valueSet) [(v, target - v) | v <- values]
    where
        valueSet = S.fromList values

getTriplesWithSum :: [Int] -> Int -> [(Int, Int, Int)]
getTriplesWithSum values target = [(v1, v2, v3) | v1 <- values, v2 <- values, v3 <- values, sum [v1, v2, v3] == target, v1 /= v2, v2 /= v3, v3 /= v1]

getTriplesWithSumFast :: [Int] -> Int -> [(Int, Int, Int)]
getTriplesWithSumFast values target = filter (\(_, _, v3) -> S.member v3 valueSet) [(v1, v2, target - v1 - v2) | v1 <- values, v2 <- values]
    where
        valueSet = S.fromList values

main = do
    contents <- readFile "inputs/1.txt"
    let values = map read (lines contents)
    let (p1, p2):_ = values `getPairsWithSumFast` 2020
    let (t1, t2, t3):_ = values `getTriplesWithSumFast` 2020
    print (p1 * p2, t1 * t2 * t3)