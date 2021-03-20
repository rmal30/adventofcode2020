import qualified Data.Set as S
parseLetter l = case l of
    'F' -> 0
    'B' -> 1
    'L' -> 0
    'R' -> 1

getValue k [] = 0
getValue k (a:x) = a + k*(getValue k x)

parseBinary = (getValue 2) . reverse
parseSeat x = quotRem (parseBinary (map parseLetter x)) 8
getSeatId (a,b) = a*8 + b

main = do
    input <- readFile "inputs/5.txt"
    let rawSeats = lines input
    let seatIds = map (getSeatId . parseSeat) rawSeats
    let part1 = maximum seatIds
    let seatSet = S.fromList seatIds
    let part2 = head (filter (\i -> not (S.member i seatSet) && S.member (i-1) seatSet && S.member (i + 1) seatSet) [1..1022])
    print (part1, part2)