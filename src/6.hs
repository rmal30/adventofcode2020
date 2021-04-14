import qualified Data.Set as S
import Data.List (foldl', foldl1')
import Utils(split)

main :: IO ()
main = do
    contents <- readFile "inputs/6.txt"
    let groups = "" `split` lines contents
    let groupsOfSets = map (map S.fromList) groups
    let part1 = sum [S.size (foldl' S.union S.empty groupOfSets) | groupOfSets <- groupsOfSets]
    let part2 = sum [S.size (foldl1' S.intersection groupOfSets) | groupOfSets <- groupsOfSets]
    print (part1, part2)