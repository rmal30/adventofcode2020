import qualified Data.Set as S
import Data.List (foldl')
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split tok arr =
    if not (null y) then
        x:(split tok newArr)
    else
        [x]
    where
        (x, y) = break (==tok) arr
        newArr = tail y

union x = foldl' S.union S.empty x
intersection x = foldl' S.intersection (head x) x

main = do
    contents <- readFile "inputs/6.txt"
    let groups = (split "" (lines contents))
    let part1 = sum [let sets = map (S.fromList) g in S.size (union sets) | g <- groups]
    let part2 = sum [let sets = map (S.fromList) g in S.size (intersection sets) | g <- groups]
    print (part1, part2)