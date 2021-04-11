import Utils(split)

parseRecord :: String -> (String, [(Int, Int)])
parseRecord r = (field, map (\i -> let [low,high] = map read (split '-' i) in (low, high)) [r1, r2])
    where
        [field, _:ranges] = split ':' r
        [r1, _, r2] = split ' ' ranges

getTicketValues :: String -> [Int]
getTicketValues x = map read (split ',' x)

inRange x range = any (\(low, high) -> (x >= low && x <= high)) range
anyInRange ranges x = any (inRange x) ranges

getInvalidValues ranges x = filter (not . (anyInRange ranges)) x

getMatches records tickets = [map fst (filter (\(n, v) -> all (\t -> inRange (t !! i) v) tickets) [(n, v) | (n, v) <- records]) | i <- [0..(cols-1)]]
    where
        cols = length (head tickets)

simplify :: ([(String, Int)], [[String]]) -> ([(String, Int)], [[String]])
simplify (m, x) = ((u, p):m, map (\i -> if null (tail i) then i else (filter (/=u) i)) x)
    where
        (p, u:_) = head (filter (\(_, i) -> (null (tail i)) && not (elem (head i) (map fst m))) (zip [0..] x))

main = do
    contents <- readFile "inputs/16.txt"
    let [recordsStr, _:ticketStr:_, _:nearbyStr] = split "" (split '\n' contents)
    let records = map parseRecord recordsStr
    let nearbyTickets = map getTicketValues nearbyStr
    let ranges = map snd records
    let part1 = sum (map (sum . (getInvalidValues ranges)) nearbyTickets)
    let validTickets = filter (\i -> length (getInvalidValues ranges i) == 0) nearbyTickets
    let matches = getMatches records validTickets
    let (reducedMatches, _) = head (dropWhile (\(i, _) -> length i < length matches) (iterate simplify ([], matches)))
    let ticket = getTicketValues ticketStr
    let part2 = product (map ((ticket!!) . snd) (filter (\(i,_) -> head (split ' ' i) == "departure") reducedMatches))
    print (part1, part2)