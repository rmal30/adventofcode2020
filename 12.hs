import Data.List(foldl')
import Data.Map.Strict(fromList, (!), Map)

data Action = N | S | E | W | L | R | F deriving (Read, Show)
type Position = (Int, Int)
type Bearing = Int

bearingVectorMap :: Map Bearing Position
bearingVectorMap = fromList [(0, (0, 1)), (90, (1, 0)), (180, (0, -1)), (270, (-1, 0)), (360, (0, 1))]

bearingMult :: Position -> Position -> Position
bearingMult (x1, y1) (x2, y2) = (x2 * y1 + x1 * y2, y1 * y2 - x1 * x2)

moveForward :: Position -> Bearing -> Int -> Position
moveForward (x, y) angle value = (x + dx * value, y + dy * value)
    where
        (dx, dy) = bearingVectorMap ! angle

turnPosition :: Position -> Bearing -> Position
turnPosition (x, y) angle = (x, y) `bearingMult` (bearingVectorMap ! angle)

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
        F -> ((x + wx * value, y + wy * value), (wx, wy))

parseCommand :: String -> Maybe (Action, Int)
parseCommand (a:v) = Just (read [a], read v)
parseCommand [] = Nothing

manhattan :: Num a => (a, a) -> a
manhattan (x, y) = abs x + abs y

main :: IO ()
main = do
    contents <- readFile "inputs/12.txt"
    let commands = mapMaybe parseCommand (lines contents)
    let (pos1, _) = foldl' applyAction ((0, 0), 90) commands
    let (pos2, _) = foldl' applyAction2 ((0, 0), (10, 1)) commands
    print (manhattan pos1, manhattan pos2)