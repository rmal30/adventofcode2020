
split [] _ = []
split arr tok =
    if not (null y) then
        x:(split newArr tok)
    else
        [x]
    where
        (x, y) = break (==tok) arr
        newArr = tail y

parseLine :: String -> ((Int, Int), Char, String)
parseLine str = ((low, high), l, p)
    where
        [r, l:_, p] = split str ' '
        [low, high] = map read (split r '-')

checkPassword ((low, high), letter, password) = low <= count && count <= high where count = length (filter (==letter) password)
checkPassword2 ((low, high), letter, password) = (a && (not b)) || (b && (not a))
        where
            a = password !! (low - 1) == letter
            b = password !! (high - 1) == letter

main = do
    contents <- readFile "inputs/2.txt"
    let values = map parseLine (lines contents)
    let part1 = length (filter checkPassword values)
    let part2 = length (filter checkPassword2 values)
    print (part1, part2)

