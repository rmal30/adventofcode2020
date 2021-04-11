import Data.Array.IArray(array, bounds, (!), indices, elems, Array)
import Utils(takeWhileChange)
deltas = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

nextStep :: Array (Int, Int) Char -> (Int, Int) -> Char
nextStep grid (i, j) =
    case grid ! (i, j) of
        '.' -> '.'
        'L' -> if neighbourCount == 0 then '#' else 'L'
        '#' -> if neighbourCount >= 4 then 'L' else '#'

    where neighbourCount = length (filter (=='#') [let
            u = safeLookup grid (d1 + i, d2 + j) in
                case u of
                    Just s -> s
                    Nothing -> 'L'
                 | (d1, d2) <- deltas])

nextStep2 :: Array (Int, Int) Char -> (Int, Int) -> Char
nextStep2 grid (i, j) =
    case grid ! (i, j) of
        '.' -> '.'
        'L' -> if neighbourCount == 0 then '#' else 'L'
        '#' -> if neighbourCount >= 5 then 'L' else '#'

    where neighbourCount = length (filter (=='#') [let
            u = head (dropWhile (== Just '.') (map (\n -> safeLookup grid (d1*n + i, d2*n + j)) [1..])) in
                case u of
                    Just s -> s
                    Nothing -> 'L'
                 | (d1, d2) <- deltas])

safeLookup grid (a,b) = if a >= alow && a <= ahigh && b >= blow && b <= bhigh then Just (grid ! (a, b)) else Nothing
    where ((alow, blow), (ahigh, bhigh)) = bounds grid

nextIteration :: Array (Int, Int) Char -> Array (Int, Int) Char
nextIteration grid = array (bounds grid) [((i, j), nextStep grid (i, j)) | (i, j) <- indices grid]

nextIteration2 :: Array (Int, Int) Char -> Array (Int, Int) Char
nextIteration2 grid = array (bounds grid) [((i, j), nextStep2 grid (i, j)) | (i, j) <- indices grid]

countFilled :: Array (Int, Int) Char -> Int
countFilled grid = length (filter (=='#') (elems grid))

main = do
    contents <- readFile "inputs/11.txt"
    let gridStr = lines contents
    let grid = array ((0, 0), (length (gridStr !! 0) - 1, length gridStr - 1)) [((x, y), c) | (y, l) <- zip [0..] gridStr, (x, c) <- zip [0..] l]
    let part1 = last (takeWhileChange (map countFilled (iterate nextIteration grid)))
    let part2 = last (takeWhileChange (map countFilled (iterate nextIteration2 grid)))
    print (part1, part2)