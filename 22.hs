import qualified Data.Set as S
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split tok arr =
    if not (null y) then
        x:(split tok newArr)
    else
        [x]
    where
        (x, y) = break (==tok) arr
        newArr = tail y

playRound (a:as, b:bs) =
    if a > b then
        (as ++ [a, b], bs)
    else
        (as, bs ++ [b, a])

playRound2 (history, a:as, b:bs) =
    if firstWins then
        (newHistory, as ++ [a, b], bs)
    else
        (newHistory, as, bs ++ [b, a])
    where
        newHistory = S.insert (a:as, b:bs) history
        (winner, _) = playCombat2 (take a as, take b bs)
        firstWins =
            if a <= length as && b <= length bs then
                winner == 1
            else
                a > b

getScore x = sum (zipWith (*) (reverse x) [1..])

playCombat (deck1, deck2) =
        if null a then
            (2, getScore b)
        else
            (1, getScore a)
    where
        (a, b) = head (dropWhile (\(i, j) -> not (null i) && not (null j)) (iterate playRound (deck1, deck2)))

playCombat2 (deck1, deck2) =
        if null b || (S.member (a, b) h) then
            (1, getScore a)
        else
            (2, getScore b)
    where
        (h, a, b) = head (dropWhile (\(h, i, j) -> not (null i) && not (null j) && not (S.member (i, j) h)) (iterate playRound2 (S.empty, deck1, deck2)))

parseDeck :: [String] -> [Int]
parseDeck = map read

main = do
    contents <- readFile "inputs/22.txt"
    let [_:deck1Str, _:deck2Str] = split "" (lines contents)
    let (deck1, deck2) = (parseDeck deck1Str, parseDeck deck2Str)
    let (_, part1) = playCombat (deck1, deck2)
    let (_, part2) = playCombat2 (deck1, deck2)
    print (part1, part2)