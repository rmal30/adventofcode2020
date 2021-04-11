import Data.Map ((!), fromList, member, keys)
import qualified Data.Set as S
import Utils(split, join)

expectedFields = keys rules

eyeColors = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

rules = fromList [
    ("byr", \yearStr -> let birthYear = read yearStr in (birthYear >= 1920 && birthYear <= 2002)),
    ("iyr", \yearStr -> let issueYear = read yearStr in (issueYear >= 2010 && issueYear <= 2020)),
    ("eyr", \yearStr -> let expireYear = read yearStr in (expireYear >= 2020 && expireYear <= 2030)),
    ("hgt", \heightStr -> let
                    (heightValueStr, unit) = splitAt (length heightStr - 2) heightStr
                    height = read heightValueStr
                    validCm = (unit == "cm" && height >= 150 && height <= 193)
                    validIn = (unit == "in" && height >= 59 && height <= 76)
                in validCm || validIn
            ),
    ("hcl", \color -> length color == 7 && head color == '#' && all (\i -> elem i (['0'..'9'] ++ ['a'..'f'])) (tail color)),
    ("ecl", \color -> S.member color eyeColors),
    ("pid", \passportId -> length passportId == 9 && all (\i -> elem i ['0'..'9']) passportId)
    ]

main = do
    contents <- readFile "inputs/4.txt"
    let values = lines contents
    let passports = [fromList [let [a, b] = (split ':' fv) in (a,b) | fv <- p] | p <- map (split ' ') (map (join ' ') (split "" values))]
    let part1 = length (filter (\m -> all (\i -> member i m) expectedFields) passports)
    let part2 = length (filter (\m -> all (\i -> member i m && (rules ! i) (m ! i)) expectedFields) passports)
    print (part1, part2)