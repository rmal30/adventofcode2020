import Data.Array.IArray((!), array, Array)
getWrappedCoordinatesFromLine (x, y) (a, b) = [(rem (a * i) x, b * i) | i <- takeWhile (\i -> b * i < y) [0..]]

getCountFromCoordinates :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Int
getCountFromCoordinates values (sy, sx) (dx,dy) = length (filter (=='#') [values ! (y, x) | (x, y) <- getWrappedCoordinatesFromLine (sx, sy) (dx, dy)])

parseGrid :: String -> ((Int, Int), Array (Int, Int) Char)
parseGrid gridStr = ((rowCount, colCount), values)
    where
        rows = lines gridStr
        (rowCount, colCount) = (length rows, length (head rows))
        values = array ((0, 0), (rowCount - 1, colCount - 1)) [((r, c), letter) | (r, row) <- zip [0..] rows, (c, letter) <- zip [0..] row]

main = do
    contents <- readFile "inputs/3.txt"
    let ((rowCount, colCount), grid) = parseGrid contents
    let part1 = (getCountFromCoordinates grid (rowCount, colCount) (3, 1))
    let part2 = (product (map (getCountFromCoordinates grid (rowCount, colCount)) [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)]))
    print (part1, part2)