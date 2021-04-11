import Data.Array.IArray(array, Array, bounds, inRange, (!), elems, Ix)
import Utils(split)

conwayLogic :: Ix a => Array a Char -> a -> [a] -> Char
conwayLogic volume index neighbourCells =
    if inRange (bounds volume) index && volume ! index == '#' then
        if neighbourCount == 2 || neighbourCount == 3 then
            '#'
        else
            '.'
    else
        if neighbourCount == 3 then
            '#'
        else
            '.'
    where
        neighbourCount = length (filter (=='#') (map (volume!) (filter (\i -> inRange (bounds volume) i) neighbourCells)))

neighbours3 = [(i, j, k) | i <- [(-1)..(1)], j <- [(-1)..(1)], k <- [(-1)..(1)], (i, j, k) /= (0, 0, 0)]
neighbours4 = [(i, j, k, l) | i <- [(-1)..(1)], j <- [(-1)..(1)], k <- [(-1)..(1)], l <- [(-1)..(1)], (i, j, k, l) /= (0, 0, 0, 0)]

conway3 :: Array (Int, Int, Int) Char -> (Int, Int, Int) -> Char
conway3 volume (i, j, k) = conwayLogic volume (i, j, k) [(i + di, j + dj, k + dk) | (di, dj, dk) <- neighbours3]

conway4 :: Array (Int, Int, Int, Int) Char -> (Int, Int, Int, Int) -> Char
conway4 volume (i, j, k, l) = conwayLogic volume (i, j, k, l) [(i + di, j + dj, k + dk, l + dl) | (di, dj, dk, dl) <- neighbours4]

getNextNeighbours3 :: Array (Int, Int, Int) Char -> Array (Int, Int, Int) Char
getNextNeighbours3 volume = array ((la, lb, lc), (ha + 2, hb + 2, hc + 2)) [((i, j, k), conway3 volume (i - 1, j - 1, k - 1)) | i <- [la..(ha + 2)], j <- [lb..(hb + 2)], k <- [lc..(hc + 2)]]
    where
        ((la, lb, lc), (ha, hb, hc)) = bounds volume

getNextNeighbours4 :: Array (Int, Int, Int, Int) Char -> Array (Int, Int, Int, Int) Char
getNextNeighbours4 volume = array ((la, lb, lc, ld), (ha + 2, hb + 2, hc + 2, hd + 2)) [((i, j, k, l), conway4 volume (i - 1, j - 1, k - 1, l - 1)) | i <- [la..(ha + 2)], j <- [lb..(hb + 2)], k <- [lc..(hc + 2)], l <- [ld..(hd + 2)]]
    where
        ((la, lb, lc, ld), (ha, hb, hc, hd)) = bounds volume

parseVolume3 :: String -> Array (Int, Int, Int) Char
parseVolume3 contents = array ((0, 0, 0), (length volList - 1, length (head volList) - 1, length (head (head volList)) - 1)) [((i, j, k), letter) | (i, plane) <- zip [0..] volList, (j, row) <- zip [0..] plane, (k, letter) <- zip [0..] row]
    where
        volList = [split '\n' contents]

parseVolume4 :: String -> Array (Int, Int, Int, Int) Char
parseVolume4 contents = array ((0, 0, 0, 0), (length volList - 1, length (head volList) - 1, length (head (head volList)) - 1, length (head (head (head volList))) - 1)) [((i, j, k, l), letter) | (i, vol) <- zip [0..] volList, (j, plane) <- zip [0..] vol, (k, row) <- zip [0..] plane, (l, letter) <- zip [0..] row]
    where
        volList = [[split '\n' contents]]

countActive :: Ix a => Array a Char -> Int
countActive x = length (filter (=='#') (elems x))

main = do
    contents <- readFile "inputs/17.txt"
    let part1 = (map countActive (iterate getNextNeighbours3 (parseVolume3 contents))) !! 6
    let part2 = (map countActive (iterate getNextNeighbours4 (parseVolume4 contents))) !! 6
    print (part1, part2)