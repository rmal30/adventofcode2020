import qualified Data.Set as S
import Data.List(scanl')
getSums x = S.fromList [a + b | a <- x, b <- x, a /= b]
series = scanl' (+) 0

parseInt :: String -> Integer
parseInt = read

main = do
    contents <- readFile "inputs/9.txt"
    let numbers = map parseInt (lines contents)
    let partialSums = series numbers

    let (_, part1) = (head (dropWhile (\(i, v) -> (S.member v (getSums (take 25 (drop (i - 25) numbers))))) (zip [25..(length numbers - 1)] (drop 25 numbers))))
    let (i, j, a, b) = head (filter (\(_, _, a, b) -> a - b == part1) [(i, j, a, b) | (i, a) <- zip [0..] partialSums, (j, b) <- takeWhile(\(j, _) -> j < i) (zip [0..] partialSums)])
    let conseqItems = drop j (take i numbers)
    let part2 = minimum conseqItems + maximum conseqItems
    print (part1, part2)
