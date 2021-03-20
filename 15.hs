
import Data.Foldable(foldlM)
import Data.List(foldl')
import qualified Data.IntMap.Strict as M
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST
import Control.Monad.ST

startingValues = [6,4,12,1,20,0,16]

getNth input n = foldl' (\(l, heard) i -> (i - M.findWithDefault i l heard, M.insert l i heard)) initial [(length input - 1)..(n - 2)]
    where
        initial = (last input, M.fromList (init (zip input [0..])))

getNthFast :: [Int] -> Int -> Int
getNthFast input n = runST thread
    where
        thread = do
            initial <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
            _ <- mapM_ (\(i, j) -> writeArray initial i j) (init (zip input [0..]))
            (a,b) <- foldlM (\(l, heard) i -> do
                v <- readArray heard l
                _ <- writeArray heard l i
                return (if v == -1 then 0 else i - v, heard)
                ) (last input, initial) [(length input - 1)..(n - 2)]
            return a

main = do
    let part1 = getNthFast startingValues 2020
    let part2 = getNthFast startingValues 30000000
    print (part1, part2)