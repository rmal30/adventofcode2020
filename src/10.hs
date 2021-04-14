import Data.List(sort, group, foldl')
import qualified Data.IntMap.Strict as M

addEntry :: Int -> M.IntMap Int -> Int -> M.IntMap Int
addEntry target counts previousTarget = M.insertWith (+) adapter (M.findWithDefault 0 previousTarget counts) counts

addAdapter :: M.IntMap Int -> Int -> M.IntMap Int
addAdapter counts adapter = foldl' (addEntry adapter) counts [adapter - 3, adapter - 2, adapter - 1]

tally :: Ord a => [a] -> [(a, Int)]
tally = map (\x -> (head x, length x)) . group . sort 

main :: IO ()
main = do
    contents <- readFile "inputs/10.txt"
    let adapters = sort (map read (lines contents))
    let deltas = M.fromList (tally (zipWith (-) adapters (0:adapters)))
    let part1 = (deltas M.! 1) * (1 + deltas M.! 3)
    let highest = maximum adapters
    let part2 = M.findWithDefault 0 highest (foldl' addAdapter (M.fromList [(0, 1)]) adapters)
    print (part1, part2)