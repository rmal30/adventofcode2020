import Data.Map.Strict ((!), fromList, union, Map)
import Data.List(scanl', isPrefixOf)
import Utils(split)

data Rule = Value String | Options [[Int]] deriving Show

trim :: String -> String
trim = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')

dropAfter :: Eq a => (a -> Bool) -> [a] -> [a]
dropAfter _ [] = []
dropAfter cond (x:xs) = x:(if cond x then [] else dropAfter cond xs)

parseOptions :: String -> [[Int]]
parseOptions remaining = map (map read . split ' ' . trim) (split '|' remaining)

parseRule :: String -> (Int, Rule)
parseRule ruleStr = (read ruleNo, rule)
    where
        [ruleNo, _:remaining] = split ':' ruleStr
        isValue = (' ' `notElem` remaining) && any (`elem` ['a'..'z']) remaining
        rule = if isValue then Value (init (tail remaining)) else Options (parseOptions remaining)

applyRule :: String -> [Int] -> Map Int Rule -> [String]
applyRule str rules ruleSet = last (dropAfter null (scanl' (applyRuleToStrings ruleSet) [str] rules))

applyRuleToStrings :: Map Int Rule -> [String] -> Int -> [String]
applyRuleToStrings ruleSet strings rule = [newStr | str <- strings, newStr <- getNextStrings str]
    where
        getNextStrings str =
            case ruleSet ! rule of
                Value v -> [drop (length v) str | v `isPrefixOf` str]
                Options o -> [string | option <- o, string <- applyRule str option ruleSet]

testRule :: Rule -> Map Int Rule -> String -> Bool
testRule (Value _) _ _ = False
testRule (Options rule) ruleSet str = any (\i -> "" `elem` applyRule str i ruleSet) rule

main :: IO ()
main = do
    contents <- readFile "inputs/19.txt"
    let [rulesStr, strings] = split "" (lines contents)
    let ruleSet = fromList (map parseRule rulesStr)
    let part1 = length (filter (testRule (ruleSet ! 0) ruleSet) strings)
    let ruleSet2 = fromList [(8, Options [[42], [42, 8]]), (11, Options [[42, 31], [42, 11, 31]])] `union` ruleSet
    let part2 = length (filter (testRule (ruleSet2 ! 0) ruleSet2) strings)
    print (part1, part2)