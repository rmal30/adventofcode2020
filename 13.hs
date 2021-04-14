import Utils(split)
import Data.List(foldl')
import Data.Maybe(catMaybes)

readBusId :: (Read a, Integral a) => String -> Maybe a
readBusId "x" = Nothing
readBusId busIdString = Just (read busIdString)

extendedEuclidean :: Integral a => a -> a -> (a, a, a, a, a, a)
extendedEuclidean a b = head (dropWhile (\( _, _, _, x, _, _) -> x /= 0) (iterate stateFunc initial))
    where
        stateFunc (r0, s0, t0, r1, s1, t1) = let q = div r0 r1 in (r1, s1, t1, r0 - q * r1, s0 - q * s1, t0 - q * t1)
        initial = (a, 1, 0, b, 0, 1)

solveCRT :: Integral a => (a, a) -> (a, a) -> (a, a)
solveCRT (p1, r1) (p2, r2) = (p1 * p2, r3)
    where
        (_, m1, m2, _, _, _) = extendedEuclidean p1 p2
        r3 = mod (r1 * p2 * m2 + r2 * p1 * m1) (p1 * p2)

solveCRTGeneral :: Integral a => [(a, a)] -> (a, a)
solveCRTGeneral = foldl' solveCRT (1, 0)

main :: IO ()
main = do
    contents <- readFile "inputs/13.txt"
    let [timeStr, busesStr] = lines contents
    let timeValue = read timeStr :: Integer
    let allBuses = map readBusId (split ',' busesStr)
    let buses = catMaybes allBuses
    let (_, part1) = minimum [
            let 
                waitTime = bus - rem timeValue bus 
            in 
                (waitTime, waitTime * bus) | bus <- buses]
    let (_, part2) = solveCRTGeneral [(busId, mod (busId - busIndex) busId) | (busIndex, Just busId) <- zip [0..] allBuses]
    print (part1, part2)