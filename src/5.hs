import qualified Data.Set as S
import qualified Data.Map.Strict as M

binaryCodes :: M.Map Char Int
binaryCodes = M.fromList [('F', 0), ('B', 1), ('L', 0), ('R', 1)]

getValueFromBase :: Integral a => Int -> [Int] -> a
getValueFromBase _ [] = 0
getValueFromBase base (d:ds) = fromIntegral d + fromIntegral base * getValueFromBase base ds

binaryToDecimal :: [Int] -> Integer
binaryToDecimal = getValueFromBase 2 . reverse

parseSeat :: [Char] -> (Integer, Integer)
parseSeat seatCode = quotRem seatNumber 8
    where
        seatBinary = map (binaryCodes M.!) seatCode
        seatNumber = binaryToDecimal seatBinary

getSeatId :: (Integer, Integer) -> Integer
getSeatId (row, column) = (row * 8) + column

main :: IO ()
main = do
    input <- readFile "inputs/5.txt"
    let rawSeats = lines input
    let seatIds = map (getSeatId . parseSeat) rawSeats
    let part1 = maximum seatIds
    let seatSet = S.fromList seatIds
    let part2:_ = filter (\seat -> not (S.member seat seatSet) && S.member (seat - 1) seatSet && S.member (seat + 1) seatSet) [1..]
    print (part1, part2)