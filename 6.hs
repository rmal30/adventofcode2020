import qualified Data.Set as S
import Data.List (foldl', foldl1')
import Utils(split)

main = do
    contents <- readFile "inputs/6.txt"
    let groups = "" `split` (lines contents)
    let part1 = sum [let sets = map (S.fromList) g in S.size (foldl' S.union S.empty sets) | g <- groups]
    let part2 = sum [let sets = map (S.fromList) g in S.size (foldl1' S.intersection sets) | g <- groups]
    print (part1, part2)