import Data.Map.Strict ((!), fromList, insertWith, empty, elems, filter, Map, keys)
import Data.List(foldl')
import qualified Data.Set as S

type Vector2D = (Int, Int)
data Direction = E | W | NE | NW | SE | SW deriving (Show, Ord, Eq)

bearingVectorMap :: Map Direction Vector2D
bearingVectorMap = fromList [(E, (1, 0)), (W, (-1, 0)), (NE, (1, 1)), (NW, (0, 1)), (SE, (0, -1)), (SW, (-1, -1))]

add :: Vector2D -> Vector2D -> Vector2D
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

parseDirections :: String -> [Direction]
parseDirections [] = []
parseDirections ('e' : line) = E : parseDirections line
parseDirections ('w' : line) = W : parseDirections line
parseDirections ('n' : 'e' : line) = NE : parseDirections line
parseDirections ('n' : 'w' : line) = NW : parseDirections line
parseDirections ('s' : 'e' : line) = SE : parseDirections line
parseDirections ('s' : 'w' : line) = SW : parseDirections line
parseDirections _ = []

getPosition :: [Direction] -> Vector2D
getPosition = foldl' (\position direction -> position `add` (bearingVectorMap ! direction)) (0, 0)

conwayLogic :: Bool -> Int -> Bool
conwayLogic True neighbourCount = not (neighbourCount == 0 || neighbourCount > 2)
conwayLogic False neighbourCount = neighbourCount == 2

unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

getNearbyTiles :: S.Set Vector2D -> [Vector2D]
getNearbyTiles tiles = unique [tile `add` nearbyVectors | tile <- S.toList tiles, nearbyVectors <- (0, 0) : elems bearingVectorMap]

getNeighbourCount :: Vector2D -> S.Set Vector2D -> Int
getNeighbourCount pos tiles = length (Prelude.filter (`S.member` tiles) (map (add pos) (elems bearingVectorMap)))

nextStep :: S.Set Vector2D -> S.Set Vector2D
nextStep tiles = S.fromList (Prelude.filter (\tile -> conwayLogic (S.member tile tiles) (getNeighbourCount tile tiles)) (getNearbyTiles tiles))

tally :: Ord a => [a] -> Map a Int
tally = foldl' (\currentTally key -> insertWith (+) key 1 currentTally) empty

main :: IO ()
main = do
    contents <- readFile "inputs/24.txt"
    let flipPositions = map (getPosition . parseDirections) (lines contents)
    let currentTiles = S.fromList (keys (Data.Map.Strict.filter odd (tally flipPositions)))
    let part1 = S.size currentTiles
    let tileIterations = iterate nextStep currentTiles
    let part2 = S.size (tileIterations !! 100)
    print (part1, part2)