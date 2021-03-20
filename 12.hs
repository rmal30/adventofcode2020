import Data.List(foldl', scanl')

data Action = N | S | E | W | L | R | F deriving (Read, Show)
type Position = (Int, Int)
type Bearing = Int

moveForward :: Position -> Bearing -> Int -> Position
moveForward (x, y) angle value =
    case angle of
        0 -> (x, y + value)
        90 -> (x + value, y)
        180 -> (x, y - value)
        270 -> (x - value, y)

turnPosition (x, y) angle =
    case angle of
        0 -> (x, y)
        90 -> (y, -x)
        180 -> (-x, -y)
        270 -> (-y, x)
        360 -> (x, y)

applyAction :: (Position, Bearing) -> (Action, Int) -> (Position, Bearing)
applyAction ((x, y), angle) (action, value) =
    case action of
        N -> ((x, y + value), angle)
        E -> ((x + value, y), angle)
        W -> ((x - value, y), angle)
        S -> ((x, y - value), angle)
        L -> ((x, y), mod (angle - value) 360)
        R -> ((x, y), mod (angle + value) 360)
        F -> (moveForward (x, y) angle value, angle)

applyAction2 :: (Position, Position) -> (Action, Int) -> (Position, Position)
applyAction2 ((x, y), (wx, wy)) (action, value) =
    case action of
        N -> ((x, y), (wx, wy + value))
        E -> ((x, y), (wx + value, wy))
        W -> ((x, y), (wx - value, wy))
        S -> ((x, y), (wx, wy - value))
        L -> ((x, y), turnPosition (wx, wy) (360 - value))
        R -> ((x, y), turnPosition (wx, wy) value)
        F -> ((x + wx*value, y + wy*value), (wx, wy))


parseCommand :: String -> (Action, Int)
parseCommand (a:v) = (read [a], read v)

manhattan (x, y) = abs x + abs y

main = do
    contents <- readFile "inputs/12.txt"
    let commands = map parseCommand (lines contents)
    let (pos1, _) = foldl' applyAction ((0, 0), 90) commands
    let (pos2, _) = foldl' applyAction2 ((0, 0), (10, 1)) commands
    print (manhattan pos1, manhattan pos2)