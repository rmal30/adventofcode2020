import Data.List(scanl', foldl')
import Data.Map((!), fromList)

import Utils(split)

splitOn cond [] = []
splitOn cond x = if null b then [a] else a:(splitOn cond (tail b))
    where
        (a, b) = break cond x

splitOnInclusive cond [] = []
splitOnInclusive cond x = if null b then [a] else a:[head b]:(splitOnInclusive cond (tail b))
    where
        (a, b) = break cond x

data Op = Plus | Times deriving Show

ops = fromList [('+', Plus), ('*', Times)]

getRemaining [] = []
getRemaining ((o:_):y:xs) = (ops ! o, y) : getRemaining xs

parseExpression (x:xs) = (x, getRemaining xs)

applyOperations :: (String -> Int) -> (String, [(Op, String)]) -> Int
applyOperations parseFunc (initial, operations) = foldl' (\x (op, str) ->
    let v = parseFunc str in
        case op of
            Times -> x*v
            Plus -> x + v
    ) initialValue operations
    where
        initialValue =
            if any (\i -> elem i "*+") initial then
                parseFunc initial
            else
                read initial

getExpressions x = splitOnInclusive (\i -> elem i "+*") h ++ simplifiedR
    where
        bracketLevels = (scanl' (\counter c -> if c == '(' then (counter + 1) else if c == ')' then (counter - 1) else counter) 0 x)
        h:subExpressions = map (\i -> map (\(s, _, _) -> s) i) (splitOn (\(_, i, j) -> (i, j) == (1, 0) || (i, j) == (0, 1)) (zip3 x bracketLevels (tail bracketLevels)))
        simplifiedR = concat (map (\i -> if elem (head i) "+*" || elem (last i) "+*" then filter (/="") (splitOnInclusive (\c -> elem c "+*") i) else [i]) subExpressions)

getValue :: String -> Int
getValue = (applyOperations getValue) . parseExpression . getExpressions

getValue2 :: String -> Int
getValue2 = product . (map ((applyOperations getValue2) . parseExpression)) . (split "*") . getExpressions

main = do
    contents <- readFile "inputs/18.txt"
    let expressionStrs = map (filter (/=' ')) (lines contents)
    let part1 = sum (map getValue expressionStrs)
    let part2 = sum (map getValue2 expressionStrs)
    print (part1, part2)
