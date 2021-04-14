import Utils(split)

parseRecord :: String -> (String, [(Int, Int)])
parseRecord r = (field, [let [low,high] = map read (split '-' i) in (low, high) | i <- [r1, r2]])
    where
        [field, _:ranges] = split ':' r
        [r1, _, r2] = split ' ' ranges

getTicketValues :: String -> [Int]
getTicketValues x = map read (split ',' x)

inRange :: Integral a => a -> [(a, a)] -> Bool
inRange x = any (\(low, high) -> x >= low && x <= high)

anyInRange :: Integral a => [[(a, a)]] -> a -> Bool
anyInRange ranges x = any (inRange x) ranges

getInvalidValues :: Integral a => [[(a, a)]] -> [a] -> [a]
getInvalidValues ranges = filter (not . anyInRange ranges)

getMatches :: Integral a => [(String, [(a, a)])] -> [[a]] -> [[String]]
getMatches records tickets = [map fst (filter (\(_, v) -> all (\t -> inRange (t !! i) v) tickets) records) | i <- [0..(cols - 1)]]
    where
        cols = length (head tickets)

simplify :: ([(String, Int)], [[String]]) -> ([(String, Int)], [[String]])
simplify (m, x) = ((u, p):m, [if null (tail i) then i else filter (/=u) i | i <- x])
    where
        (p, u:_):_ = filter (\(_, i) -> null (tail i) && notElem (head i) (map fst m)) (zip [0..] x)

main :: IO ()
main = do
    contents <- readFile "inputs/16.txt"
    let [recordsStr, _:ticketStr:_, _:nearbyStr] = split "" (split '\n' contents)
    let records = map parseRecord recordsStr
    let nearbyTickets = map getTicketValues nearbyStr
    let ranges = map snd records
    let part1 = sum (map (sum . getInvalidValues ranges) nearbyTickets)
    let validTickets = filter (null . getInvalidValues ranges) nearbyTickets
    let matches = getMatches records validTickets
    let (reducedMatches, _):_ = dropWhile (\(i, _) -> length i < length matches) (iterate simplify ([], matches))
    let ticket = getTicketValues ticketStr
    let part2 = product (map ((ticket!!) . snd) (filter (\(i,_) -> head (split ' ' i) == "departure") reducedMatches))
    print (part1, part2)