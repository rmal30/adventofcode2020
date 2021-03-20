import Data.Map ((!), fromList, member)
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

join _ [] = []
join tok (x:arr) = x ++ (if null arr then [] else ([tok] ++ join tok arr))


expectedFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

eyeColor = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

rules = fromList [
    ("byr", \x -> let u = read x in (u >= 1920 && u <= 2002)),
    ("iyr", \x -> let u = read x in (u >= 2010 && u <= 2020)),
    ("eyr", \x -> let u = read x in (u >= 2020 && u <= 2030)),
    ("hgt", \x -> let
                    (valueStr, unit) = splitAt (length x - 2) x
                    value = read valueStr
                    validCm = (unit == "cm" && value >= 150 && value <= 193)
                    validIn = (unit == "in" && value >= 59 && value <= 76)
                in validCm || validIn
            ),
    ("hcl", \x -> length x == 7 && head x == '#' && all (\i -> elem i (['0'..'9'] ++ ['a'..'f'])) (tail x)),
    ("ecl", \x -> S.member x eyeColor),
    ("pid", \x -> length x == 9 && all (\i -> elem i ['0'..'9']) x)
    ]

checkKeysInMap x m = all (\i -> member i m) x
checkKeysInMap2 x m = all (\i -> member i m && (rules ! i) (m ! i)) x

main = do
    contents <- readFile "inputs/4.txt"
    let values = lines contents
    let passports = [ fromList [let [a, b] = (split ':' fv) in (a,b) | fv <- p] | p <- map (split ' ') (map (join ' ') (split "" values))]
    let part1 = length (filter (checkKeysInMap expectedFields) passports)
    let part2 = length (filter (checkKeysInMap2 expectedFields) passports)
    print (part1, part2)