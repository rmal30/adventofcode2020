{-# LANGUAGE BangPatterns #-}
import qualified Data.IntMap.Strict as M
import Data.List(foldl')
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO
import Data.Array.Unboxed(UArray, (!))
import Data.Foldable(foldlM)

recurse n f x = foldl' (\x _ -> f x) x [1..n]

arrToCircle arr = M.fromList (zip arr (as ++ [a]))
    where
        (a:as) = arr

applyMove !maxVal !circle !current = M.union changes circle
    where
        cup1 = circle M.! current
        cup2 = circle M.! cup1
        cup3 = circle M.! cup2
        initialDestination = if current > 1 then (current - 1) else maxVal
        newDestination = head (dropWhile (\i -> i == cup1 || i == cup2 || i == cup3) (iterate (\i -> if i > 1 then i - 1 else maxVal) initialDestination))
        changes = M.fromList [(current, circle M.! cup3), (cup3, circle M.! newDestination), (newDestination, cup1)]

shuffleCups !cups !current !iterations = recurse iterations (\(!circle, !current) -> let newCircle = applyMove maxVal circle current in (newCircle, newCircle M.! current)) (cups, current)
    where
        (!maxVal, !_) = M.findMax cups

applyMoveFast maxVal circle current = do
        let initialDestination = if current > 1 then (current - 1) else maxVal
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
shuffleCupsFast cups current iterations = thread
    where
        cupCount = M.size cups
        thread = do
            initial <- newArray (0, cupCount) (-1) :: IO (IOUArray Int Int)
            _ <- mapM_ (\(i, j) -> writeArray initial i j) (M.toList cups)
            (a,b) <- foldlM (\(circle, current) _ -> do
                circle <- applyMoveFast cupCount circle current
                v <- readArray circle current
                return (circle, v)
                ) (initial, current) [1..iterations]
            na <- freeze a
            return (na, b)

main = do
    let input = [1, 5, 7, 6, 2, 3, 9, 8, 4]
    let circle = arrToCircle input
    let circle2 = arrToCircle (input ++ [10..1000000])
    let current = 1
    (newCircle, newCurrent) <- shuffleCupsFast circle current 100
    (newCircle2, newCurrent2) <- shuffleCupsFast circle2 current 10000000
    let cup1 = newCircle2 ! 1
    let cup2 = newCircle2 ! cup1
    let part2 = cup1*cup2
    print (newCircle, part2)