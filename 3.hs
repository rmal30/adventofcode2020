
coordinates (x, y) (a, b) = [(rem (a*i) x, b*i) | i <- takeWhile (\i -> b*i < y) [0..]]
getCountFromCoordinates values (sx, sy) (dx,dy) = length (filter (=='#') [(values !! y) !! x | (x, y) <- coordinates (sx, sy) (dx, dy)])


main = do
    contents <- readFile "inputs/3.txt"
    let values = lines contents
    let part1 = (getCountFromCoordinates values (length (values !! 0), length values) (3, 1))
    let part2 = (product (map (getCountFromCoordinates values (length (values !! 0), length values)) [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)]))
    print (part1, part2)