{-# LANGUAGE BangPatterns #-}
import qualified Data.IntMap.Strict as M
import Data.List(foldl')
import Data.Array.IO
import Data.Array.Unboxed(UArray, (!))
import Data.Foldable(foldlM)
import Data.Char(digitToInt)

_recurse :: Int -> (a -> a) -> a -> a
_recurse n f initial = foldl' (\x _ -> f x) initial [1..n]

arrToCircle :: [Int] -> M.IntMap Int
arrToCircle arr = M.fromList (zip arr (as ++ [a]))
    where
        (a:as) = arr

_applyMove :: Int -> M.IntMap Int -> Int -> M.IntMap Int
_applyMove !maxVal !circle !current = M.union changes circle
    where
        cup1 = circle M.! current
        cup2 = circle M.! cup1
        cup3 = circle M.! cup2
        initialDestination = if current > 1 then current - 1 else maxVal
        newDestination = head (dropWhile (\i -> i == cup1 || i == cup2 || i == cup3) (iterate (\i -> if i > 1 then i - 1 else maxVal) initialDestination))
        changes = M.fromList [(current, circle M.! cup3), (cup3, circle M.! newDestination), (newDestination, cup1)]

_shuffleCups :: M.IntMap Int -> Int -> Int -> (M.IntMap Int, Int)
_shuffleCups !cups !current !iterations = _recurse iterations (\(!circle, !current') -> let newCircle = _applyMove maxVal circle current' in (newCircle, newCircle M.! current')) (cups, current)
    where
        (!maxVal, !_) = M.findMax cups

applyMoveFast :: Int -> IOUArray Int Int -> Int -> IO (IOUArray Int Int)
applyMoveFast maxVal circle current = do
        let initialDestination = if current > 1 then current - 1 else maxVal
        cup1 <- readArray circle current
        cup2 <- readArray circle cup1
        cup3 <- readArray circle cup2
        let newDestination = head (dropWhile (\i -> i == cup1 || i == cup2 || i == cup3) (iterate (\i -> if i > 1 then i - 1 else maxVal) initialDestination))
        cup4 <- readArray circle cup3
        nextCup <- readArray circle newDestination
        _ <- writeArray circle current cup4
        _ <- writeArray circle cup3 nextCup
        _ <- writeArray circle newDestination cup1
        return circle

shuffleCupsFast :: M.IntMap Int -> Int -> Int -> IO (UArray Int Int, Int)
shuffleCupsFast cups currentCup iterations = thread
    where
        cupCount = M.size cups
        thread = do
            initial <- newArray (0, cupCount) (-1) :: IO (IOUArray Int Int)
            mapM_ (uncurry (writeArray initial)) (M.toList cups)
            (a,b) <- foldlM (\(circle, currentCup') _ -> do
                _ <- applyMoveFast cupCount circle currentCup'
                newCurrent <- readArray circle currentCup'
                return (circle, newCurrent)
                ) (initial, currentCup) [1..iterations]
            na <- freeze a
            return (na, b)

main :: IO ()
main = do
    contents <- readFile "inputs/23.txt"
    let (inputStr:_) = lines contents
    let input = map digitToInt inputStr
    let circle = arrToCircle input
    let circle2 = arrToCircle (input ++ [10..1000000])
    let current = 1
    (newCircle, _) <- shuffleCupsFast circle current 100
    (newCircle2, _) <- shuffleCupsFast circle2 current 10000000
    let cup1 = newCircle2 ! 1
    let cup2 = newCircle2 ! cup1
    let part2 = cup1 * cup2
    print (newCircle, part2)