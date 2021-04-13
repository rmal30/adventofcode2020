import Utils(split)
import Data.List(foldl')
import Data.Maybe(catMaybes)

readValue :: String -> Maybe Int
readValue "x" = Nothing
readValue x = Just (read x)

extendedEuclidean :: Integral a => a -> a -> (a, a, a, a, a, a, a, a)
extendedEuclidean a b = head (dropWhile (\(_, _, _, _, _, x, _, _) -> x /= 0) (iterate stateFunc initial))
    where
        stateFunc (_, r0, s0, t0, q1, r1, s1, t1) = let q = div r0 r1 in (q1, r1, s1, t1, q, r0 - q * r1, s0 - q * s1, t0 - q * t1)
        initial = (0, a, 1, 0, 0, b, 0, 1)

solveCRT :: Integral a => (a, a) -> (a, a) -> (a, a)
solveCRT (p1, r1) (p2, r2) = (p1 * p2, r3)
    where
        (_, _, m1, m2, _, _, _, _) = extendedEuclidean p1 p2
        r3 = mod (r1 * p2 * m2 + r2 * p1 * m1) (p1 * p2)

solveCRTGeneral :: Integral a => [(a, a)] -> (a, a)
solveCRTGeneral = foldl' solveCRT (1, 0)

main :: IO ()
main = do
    contents <- readFile "inputs/13.txt"
    let [timeStr, busesStr] = lines contents
    let timeValue = read timeStr :: Int
    let allBuses = map readValue (split ',' busesStr)
    let buses = catMaybes allBuses
    let (_, part1) = minimum [
            let 
                u = bus - rem timeValue bus 
            in 
                (u, u * bus) | bus <- buses] :: (Int, Int)
    let (_, part2) = solveCRTGeneral [ (fromIntegral v, fromIntegral (mod (v - k) v)) | (k, Just v) <- zip [0..] allBuses] :: (Integer, Integer)
    print (part1, part2)