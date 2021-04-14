import Data.Map ((!), fromList, member, keys, Map)
import qualified Data.Set as S
import Utils(split, join)

expectedFields :: [String]
expectedFields = keys rules

eyeColors :: S.Set String
eyeColors = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

rules :: Map String (String -> Bool)
rules = fromList [
    ("byr", \yearStr -> let birthYear = read yearStr :: Int in (birthYear >= 1920 && birthYear <= 2002)),
    ("iyr", \yearStr -> let issueYear = read yearStr :: Int in (issueYear >= 2010 && issueYear <= 2020)),
    ("eyr", \yearStr -> let expireYear = read yearStr :: Int in (expireYear >= 2020 && expireYear <= 2030)),
    ("hgt", \heightStr -> let
                    (heightValueStr, unit) = splitAt (length heightStr - 2) heightStr
                    height = read heightValueStr :: Int
                    validCm = (unit == "cm" && height >= 150 && height <= 193)
                    validIn = (unit == "in" && height >= 59 && height <= 76)
                in validCm || validIn
            ),
    ("hcl", \color -> length color == 7 && head color == '#' && all (`elem` (['0'..'9'] ++ ['a'..'f'])) (tail color)),
    ("ecl", `S.member` eyeColors),
    ("pid", \passportId -> length passportId == 9 && all (`elem` ['0'..'9']) passportId)
    ]

parsePassport :: [String] -> Map String String
parsePassport passportStr = fromList [(fieldName, fieldValue) | [fieldName, fieldValue] <- passportFields] 
    where
        passportFields = (map (split ':') . split ' ' . join ' ') passportStr

main :: IO ()
main = do
    contents <- readFile "inputs/4.txt"
    let passportStrs = split "" (lines contents)
    let passports = map parsePassport passportStrs
    let part1 = length (filter (\passport -> all (`member` passport) expectedFields) passports)
    let part2 = length (filter (\passport -> all (\fieldName -> member fieldName passport && (rules ! fieldName) (passport ! fieldName)) expectedFields) passports)
    print (part1, part2)