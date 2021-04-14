import Data.List(scanl')
import Data.Map((!), fromList, Map)
import Utils(split)
import Data.Foldable(foldlM)
import Text.Read(readMaybe)
import Control.Monad((<=<))

data Op = Plus | Times deriving Show

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn cond x = if null b then [a] else a : splitOn cond (tail b)
    where
        (a, b) = break cond x

splitOnInclusive :: (a -> Bool) -> [a] -> [[a]]
splitOnInclusive _ [] = []
splitOnInclusive cond x = if null b then [a] else a:[head b]: splitOnInclusive cond (tail b)
    where
        (a, b) = break cond x

charOpMap :: Map Char Op
charOpMap = fromList [('+', Plus), ('*', Times)]

getRemaining :: [String] -> Maybe [(Op, String)]
getRemaining [] = Just []
getRemaining ((opChar:_):value:xs) = do
    remaining <- getRemaining xs
    Just ((charOpMap ! opChar, value) : remaining)
getRemaining _ = Nothing

parseExpression :: [String] -> Maybe (String, [(Op, String)])
parseExpression [] = Nothing
parseExpression (x:xs) = do
    remaining <- getRemaining xs 
    Just (x, remaining)

applyOperations :: (String -> Maybe Int) -> (String, [(Op, String)]) -> Maybe Int
applyOperations parseFunc (initial, operations) = do
    initialValue <- if any (`elem` "*+") initial then parseFunc initial else readMaybe initial
    foldlM (\currentValue (op, str) -> do
        operationValue <- parseFunc str
        case op of
            Times -> Just (currentValue * operationValue)
            Plus -> Just (currentValue + operationValue)
        ) initialValue operations

getExpressions :: String -> [String]
getExpressions x = splitOnInclusive (`elem` "+*") h ++ simplifiedR
    where
        bracketLevels = scanl' (\counter c -> if c == '(' then counter + 1 else if c == ')' then counter - 1 else counter) (0 :: Int) x
        h:subExpressions = map (map (\(s, _, _) -> s)) (splitOn (\(_, i, j) -> (i, j) == (1, 0) || (i, j) == (0, 1)) (zip3 x bracketLevels (tail bracketLevels)))
        simplifiedR = concatMap (\i -> if elem (head i) "+*" || elem (last i) "+*" then filter (/="") (splitOnInclusive (`elem` "+*") i) else [i]) subExpressions

getValue :: String -> Maybe Int
getValue str = do
    let expressionStrs = getExpressions str
    parsedExpression <- parseExpression expressionStrs
    applyOperations getValue parsedExpression

getValue2 :: String -> Maybe Int
getValue2 str = do
    let expressionStrs = getExpressions str
    let productStrs =  split "*" expressionStrs
    values <- mapM (applyOperations getValue2 <=< parseExpression) productStrs
    Just (product values)

main :: IO ()
main = do
    contents <- readFile "inputs/18.txt"
    let expressionStrs = map (filter (/=' ')) (lines contents)
    let part1 = fmap sum (mapM getValue expressionStrs)
    let part2 = fmap sum (mapM getValue2 expressionStrs)
    print (part1, part2)
