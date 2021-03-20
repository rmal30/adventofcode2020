import qualified Data.Set as S
getTwoSum :: [Int] -> Int -> [(Int, Int)]
getTwoSum x k = [(i,j) | i <- x, j <- x, i + j == k, i /= j]

getTwoSumFast :: [Int] -> Int -> [(Int, Int)]
getTwoSumFast x k = filter (\(_, b) -> S.member b setValues) [(i, k - i) | i <- x]
    where
        setValues = S.fromList x

getThreeSum :: [Int] -> Int -> [(Int, Int, Int)]
getThreeSum x s = [(i, j, k) | i <- x, j <- x, k <- x, i + j + k == s, i /= j, j /= k, i /= k]

getThreeSumFast :: [Int] -> Int -> [(Int, Int, Int)]
getThreeSumFast x k = filter (\(_, _, b) -> S.member b setValues) [(i, j, k - i - j) | i <- x, j <- x]
    where
        setValues = S.fromList x

main = do
    contents <- readFile "inputs/1.txt"
    let values = map read (lines contents)
    let (a1,b1):_ = getTwoSumFast values 2020
    let (a2,b2,c2):_ = getThreeSumFast values 2020
    print (a1*b1, a2*b2*c2)