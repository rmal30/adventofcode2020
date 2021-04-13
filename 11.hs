import Data.Array.IArray(array, bounds, (!), indices, elems, Array)
import Utils(takeWhileChange)
import Data.Maybe(fromMaybe)

deltas :: [(Int, Int)]
deltas = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

data Cell = Floor | Empty | Occupied deriving Eq

parseCell :: Char -> Cell
parseCell '.' = Floor
parseCell 'L' = Empty
parseCell '#' = Occupied
parseCell _   = Floor

getNextCell :: Int -> Int -> Cell -> Cell
getNextCell neighbourLimit neighbourCount currentValue = 
    case currentValue of
        Floor -> Floor
        Empty -> if neighbourCount == 0 then Occupied else Empty
        Occupied -> if neighbourCount >= neighbourLimit then Empty else Occupied

nextStep :: Array (Int, Int) Cell -> (Int, Int) -> Cell
nextStep grid (i, j) = getNextCell 4 neighbourCount (grid ! (i, j))
    where 
        neighbourCount = length (filter (== Occupied) [fromMaybe Empty safeLookup grid (dx + i, dy + j) | (dx, dy) <- deltas])

nextStep2 :: Array (Int, Int) Cell -> (Int, Int) -> Cell
nextStep2 grid (i, j) = getNextCell 5 neighbourCount (grid ! (i, j))
    where 
        neighbourCount = length (filter (== Occupied) [
            let
                u:_ = dropWhile (== Just Floor) [safeLookup grid (dx*n + i, dy*n + j) | n <- [1..]]
            in
                fromMaybe Empty u | (dx, dy) <- deltas])

safeLookup :: Array (Int, Int) Cell -> (Int, Int) -> Maybe Cell
safeLookup grid (a,b) = 
        if a >= alow && a <= ahigh && b >= blow && b <= bhigh then 
            Just (grid ! (a, b)) 
        else 
            Nothing
    where 
        ((alow, blow), (ahigh, bhigh)) = bounds grid

nextIteration :: (Array (Int, Int) Cell -> (Int, Int) -> Cell) -> Array (Int, Int) Cell -> Array (Int, Int) Cell
nextIteration nextStepFunc grid = array (bounds grid) [((i, j), nextStepFunc grid (i, j)) | (i, j) <- indices grid]

countFilled :: Array (Int, Int) Cell -> Int
countFilled grid = length (filter (==Occupied) (elems grid))

main :: IO ()
main = do
    contents <- readFile "inputs/11.txt"
    let gridStr = lines contents
    let grid = array ((0, 0), (length (head gridStr) - 1, length gridStr - 1)) [((x, y), parseCell c) | (y, l) <- zip [0..] gridStr, (x, c) <- zip [0..] l]
    let part1 = last (takeWhileChange (map countFilled (iterate (nextIteration nextStep) grid)))
    let part2 = last (takeWhileChange (map countFilled (iterate (nextIteration nextStep2) grid)))
    print (part1, part2)