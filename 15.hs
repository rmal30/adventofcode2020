
import Data.Foldable(foldlM)
import Data.List(foldl')
import qualified Data.IntMap.Strict as M
import Data.Array.ST
import Control.Monad.ST
import Utils(split)

_getNth :: [Int] -> Int -> (Int, M.IntMap Int)
_getNth input n = foldl' (\(lastHeard, heard) i -> (i - M.findWithDefault i lastHeard heard, M.insert lastHeard i heard)) initial [(length input - 1)..(n - 2)]
    where
        initial = (last input, M.fromList (init (zip input [0..])))

getNthFast :: [Int] -> Int -> Int
getNthFast input n = runST thread
    where
        thread = do
            initial <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
            mapM_ (uncurry (writeArray initial)) (init (zip input [0..]))
            (lastNumber, _) <- foldlM (\(lastHeard, heard) i -> do
                v <- readArray heard lastHeard
                writeArray heard lastHeard i
                return (if v == -1 then 0 else i - v, heard)
                ) (last input, initial) [(length input - 1)..(n - 2)]
            return lastNumber

main :: IO ()
main = do
    contents <- readFile "inputs/15.txt"
    let startingValues = map read (split ',' (head (lines contents)))
    let part1 = getNthFast startingValues 2020
    let part2 = getNthFast startingValues 30000000
    print (part1, part2)