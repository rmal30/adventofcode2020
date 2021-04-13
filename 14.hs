import Data.IntMap(empty, insert, IntMap, elems)
import Data.List(foldl')

type MaskInstructions = (String, [(Int, Int)])

addLine ::  ([MaskInstructions], Int) -> String -> ([MaskInstructions], Int)
addLine (masks, i) line
    | takeWhile (/='=') line == "mask " = (masks ++ [(mask, [])], i + 1)
    | otherwise = (take i masks ++ [(currentMask, instrs ++ [(address, value)])], i)
    where
        address = read (takeWhile (/= ']') (tail (dropWhile (/= '[') line)))
        value = read (tail (tail (dropWhile (/= '=') line)))
        mask = tail (tail (dropWhile (/='=') line))
        (currentMask, instrs) = masks !! i

parseLines :: [String] -> [MaskInstructions]
parseLines xs = fst (foldl' addLine ([], -1) xs)

applyValueMask :: Char -> Char -> Char
applyValueMask 'X' x = x
applyValueMask x _ = x

applyAddressMask :: Char -> Char -> Char
applyAddressMask '0' x = x
applyAddressMask x _ = x

binary :: Int -> String
binary 0 = []
binary 1 = ['1']
binary x = binary (div x 2) ++ show (mod x 2)

getBinary :: Int -> String
getBinary x = reverse (take 36 (reverse (binary x) ++ repeat '0'))

getValue :: String -> Int
getValue [] = 0
getValue (x:xs) = read [x] + 2 * getValue xs

bin2dec :: String -> Int
bin2dec = getValue . reverse

getAddressesFromMask :: String -> [String]
getAddressesFromMask [] = [[]]
getAddressesFromMask ('X':x) = map ('0':) (getAddressesFromMask x) ++ map ('1':) (getAddressesFromMask x)
getAddressesFromMask (x:xs) = map (x:) (getAddressesFromMask xs)

writeMaskedValueToAddress :: String -> IntMap Int -> (Int, Int) -> IntMap Int
writeMaskedValueToAddress mask memory (address, value) = insert address maskedValue memory
    where
        maskedValue = bin2dec (zipWith applyValueMask mask (getBinary value))

writeValueToMaskedAddress :: String -> IntMap Int -> (Int, Int) -> IntMap Int
writeValueToMaskedAddress mask memory (address, value) = foldl' (\m i -> insert i value m) memory addresses
    where
        addresses = map bin2dec (getAddressesFromMask (zipWith applyAddressMask mask (getBinary address)))

executeMaskInstructions :: (String -> IntMap Int -> (Int, Int) -> IntMap Int) -> IntMap Int -> MaskInstructions -> IntMap Int
executeMaskInstructions writeFunc memory (mask, instructions) = foldl' (writeFunc mask) memory instructions

main :: IO ()
main = do
    contents <- readFile "inputs/14.txt"
    let maskMap = parseLines (lines contents)
    let part1 = sum (elems (foldl' (executeMaskInstructions writeMaskedValueToAddress) empty maskMap))
    let part2 = sum (elems (foldl' (executeMaskInstructions writeValueToMaskedAddress) empty maskMap))
    print (part1, part2)