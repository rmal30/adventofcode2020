import Data.Map(empty, insert, insertWith, Map, toList, elems, size)
import Data.List(foldl')

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

type Mask = (String, [(Int, Int)])


addLine (masks, i) line
    | takeWhile (/='=') line == "mask " = (masks ++ [(mask, [])], i + 1)
    | otherwise = (take i masks ++ [(currentMask, instrs ++ [(address, value)])], i)
    where
        address = read (takeWhile (/= ']') (tail (dropWhile (/= '[') line)))
        value = read (tail (tail (dropWhile (/= '=') line)))
        mask = tail (tail (dropWhile (/='=') line))
        (currentMask, instrs) = masks !! i

parseLines :: [String] -> [Mask]
parseLines xs = fst (foldl' addLine ([], -1) xs)


applyMask '0' _ = '0'
applyMask '1' _ = '1'
applyMask 'X' x = x

applyMask2 '0' x = x
applyMask2 '1' _ = '1'
applyMask2 'X' _ = 'X'

binary 0 = []
binary 1 = ['1']
binary x = binary (div x 2) ++ show (mod x 2)

getBinary :: Int -> String
getBinary x = reverse (take 36 (reverse (binary x) ++ repeat '0'))

getValue :: String -> Int
getValue [] = 0
getValue (x:xs) = (read [x]) + 2*(getValue xs)

bin2dec = getValue . reverse

expandAddresses [] = [[]]
expandAddresses ('0':x) = map ('0':) (expandAddresses x)
expandAddresses ('1':x) = map ('1':) (expandAddresses x)
expandAddresses ('X':x) = map ('0':) (expandAddresses x) ++ map ('1':) (expandAddresses x)

executeInstruction mask memory (address, value) = insertWith const address (bin2dec (zipWith applyMask mask (getBinary value))) memory

executeInstruction2 mask memory (address, value) = foldl' (\m i -> insertWith const i value m) memory (getAddresses mask address)

getAddresses :: String -> Int -> [Int]
getAddresses mask address = map bin2dec (expandAddresses (zipWith applyMask2 mask (getBinary address)))

executeMask :: Map Int Int -> (String, [(Int, Int)]) -> Map Int Int
executeMask memory (mask, instructions) = foldl' (executeInstruction mask) memory instructions


executeMask2 :: Map Int Int -> (String, [(Int, Int)]) -> Map Int Int
executeMask2 memory (mask, instructions) = foldl' (executeInstruction2 mask) memory instructions

main = do
    contents <- readFile "inputs/14.txt"
    let maskMap = ( (parseLines (lines contents)))
    let part1 = sum (elems (foldl' executeMask empty maskMap))
    let part2 = sum (elems (foldl' executeMask2 empty maskMap))
    print (part1, part2)