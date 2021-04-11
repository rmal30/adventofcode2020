import Data.Map.Strict ((!), fromList, insert, union)
import Data.List(scanl', isPrefixOf)
import Utils(split)

trim = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')

dropAfter :: Eq a => (a -> Bool) -> [a] -> [a]
dropAfter _ [] = []
dropAfter cond (x:xs) = x:(if cond x then [] else dropAfter cond xs)

data Rule = Value String | Options [[Int]] deriving Show

parseOptions remaining = map ((map read) . (split ' ') . trim) (split '|' remaining)

parseRule :: String -> (Int, Rule)
parseRule ruleStr = (read ruleNo, rule)
    where
        [ruleNo, _:remaining] = split ':' ruleStr
        isValue = null (filter (==' ') remaining) && any (\i -> elem i ['a'..'z']) remaining
        rule = if isValue then Value (init (tail remaining)) else Options (parseOptions remaining)

applyRule str rules ruleSet = last (dropAfter null (scanl' (applyRuleToStrings ruleSet) [str] rules))

applyRuleToStrings ruleSet strings rule = [newStr | str <- strings, newStr <- getNextStrings str]
    where
        getNextStrings str =
            case ruleSet ! rule of
                Value v -> if isPrefixOf v str then [drop (length v) str] else []
                Options o -> [string | option <- o, string <- applyRule str option ruleSet]

testRule str (Options rule) ruleSet = any (\i -> any (== "") (applyRule str i ruleSet)) rule

main = do
    contents <- readFile "inputs/19.txt"
    let [rulesStr, strings] = split "" (lines contents)
    let ruleSet = fromList (map parseRule rulesStr)
    let part1 = length (filter (\i -> testRule i (ruleSet ! 0) ruleSet) strings)
    let ruleSet2 = union (fromList [(8, Options [[42], [42, 8]]), (11, Options [[42, 31], [42, 11, 31]])]) ruleSet
    let part2 = length (filter (\i -> testRule i (ruleSet2 ! 0) ruleSet2) strings)
    print (part1, part2)