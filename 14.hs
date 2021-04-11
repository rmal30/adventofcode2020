import Data.IntMap(empty, insert, insertWith, IntMap, toList, elems, size)
import Data.List(foldl')
import Utils(split)

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

applyMask 'X' x = x
applyMask x _ = x

applyMask2 '0' x = x
applyMask2 x _ = x

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
expandAddresses ('X':x) = map ('0':) (expandAddresses x) ++ map ('1':) (expandAddresses x)
expandAddresses (x:xs) = map (x:) (expandAddresses xs)

executeInstruction mask memory (address, value) = insertWith const address (bin2dec (zipWith applyMask mask (getBinary value))) memory

executeInstruction2 mask memory (address, value) = foldl' (\m i -> insertWith const i value m) memory (getAddresses mask address)

getAddresses :: String -> Int -> [Int]
getAddresses mask address = map bin2dec (expandAddresses (zipWith applyMask2 mask (getBinary address)))

executeMask :: IntMap Int -> (String, [(Int, Int)]) -> IntMap Int
executeMask memory (mask, instructions) = foldl' (executeInstruction mask) memory instructions

executeMask2 :: IntMap Int -> (String, [(Int, Int)]) -> IntMap Int
executeMask2 memory (mask, instructions) = foldl' (executeInstruction2 mask) memory instructions

main = do
    contents <- readFile "inputs/14.txt"
    let maskMap = ( (parseLines (lines contents)))
    let part1 = sum (elems (foldl' executeMask empty maskMap))
    let part2 = sum (elems (foldl' executeMask2 empty maskMap))
    print (part1, part2)