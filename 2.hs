import Utils(split)
parsePasswordEntry :: String -> ((Int, Int), Char, String)
parsePasswordEntry str = ((low, high), letter, password)
    where
        [rangeStr, letter:_, password] = ' ' `split` str
        [low, high] = (map read . (split '-')) rangeStr

a `xor` b = (a && (not b)) || (b && (not a))

checkPasswordLetterCount ((low, high), letter, password) = low <= count && count <= high 
    where
        count = length (filter (==letter) password)

checkPasswordLetterPosition ((low, high), letter, password) = match1 `xor` match2
    where
        match1 = password !! (low - 1) == letter
        match2 = password !! (high - 1) == letter

main = do
    contents <- readFile "inputs/2.txt"
    let values = map parsePasswordEntry (lines contents)
    let part1 = length (filter checkPasswordLetterCount values)
    let part2 = length (filter checkPasswordLetterPosition values)
    print (part1, part2)

