import Data.List(sort, group, foldl')
import qualified Data.Map as M
parseInt :: String -> Int
parseInt = read

addEntry :: Int -> M.Map Int Int -> Int -> M.Map Int Int
addEntry a m b = M.insertWith (+) a (M.findWithDefault 0 b m) m

addAdapter m a = foldl' (addEntry a) m [a - 3, a - 2, a - 1]

main = do
    contents <- readFile "inputs/10.txt"
    let adapters = sort (map parseInt (lines contents))
    let deltas = map (\x -> (head x, length x)) (group (sort (zipWith (-) adapters (0:adapters))))
    let part1 = ((snd (deltas !! 0))*(1 + snd (deltas !! 1)))

    let highest = maximum adapters
    let part2 = M.findWithDefault 0 highest (foldl' addAdapter (M.fromList [(0, 1)]) adapters)
    print (part1, part2)