import Data.List(sort)
import Utils(split)

readValue :: String -> Maybe Int
readValue "x" = Nothing
readValue x = Just (read x)

catMaybes ls = [x | Just x <- ls]

extendedEuclidean a b = head (dropWhile (\(_, _, _, _, _, x, _, _) -> x /= 0) (iterate stateFunc init))
    where
        stateFunc (q0, r0, s0, t0, q1, r1, s1, t1) = let q = div r0 r1 in (q1, r1, s1, t1, q, r0 - q*r1, s0 - q*s1, t0 - q*t1)
        init = (0, a, 1, 0, 0, b, 0, 1)

solveCRT (p1, r1) (p2, r2) = (p1*p2, r3)
    where
        (_, _, m1, m2, _, _, _, _) = extendedEuclidean p1 p2
        r3 = mod (r1*p2*m2 + r2*p1*m1) (p1*p2)

solveCRTGeneral :: [(Integer, Integer)] -> (Integer, Integer)
solveCRTGeneral ((p1,r1):cs) =
    if null cs then
        (p1, r1)
    else
        solveCRT (p1, r1) (solveCRTGeneral cs)

main = do
    contents <- readFile "inputs/13.txt"
    let [timeStr, busesStr] = lines contents
    let timeValue = read timeStr :: Int
    let allBuses = (map readValue (split ',' busesStr))
    let buses = catMaybes allBuses
    let part1 = snd (minimum (map (\i -> let u = i - rem timeValue i in (u, u*i)) buses))
    let part2 = snd (solveCRTGeneral [ (fromIntegral v, fromIntegral (mod (v - k) v)) | (k, Just v) <- zip [0..] allBuses])
    print (part1, part2)