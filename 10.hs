import Data.List(sort, group, foldl')
import qualified Data.IntMap.Strict as M

addEntry :: Int -> M.IntMap Int -> Int -> M.IntMap Int
addEntry a m b = M.insertWith (+) a (M.findWithDefault 0 b m) m

addAdapter m a = foldl' (addEntry a) m [a - 3, a - 2, a - 1]

tally = map (\x -> (head x, length x)) . group . sort 

main = do
    contents <- readFile "inputs/10.txt"
    let adapters = sort (map read (lines contents))
    let deltas = M.fromList (tally (zipWith (-) adapters (0:adapters)))
    let part1 = (deltas M.! 1) *(1 + deltas M.! 3)
    let highest = maximum adapters
    let part2 = M.findWithDefault 0 highest (foldl' addAdapter (M.fromList [(0, 1)]) adapters)
    print (part1, part2)