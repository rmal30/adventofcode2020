import Data.Map.Strict ((!), fromList, insertWith, insert, empty, elems, filter, size, findWithDefault, member, Map, keys)
import Data.List(foldl')
import qualified Data.Set as S
data Direction = E | W | NE | NW | SE | SW deriving (Show, Ord, Eq)

vectors :: Map Direction (Int, Int)
vectors = fromList [
    (E, (1, 0)),
    (W, (-1, 0)),
    (NE, (1, 1)),
    (NW, (0, 1)),
    (SE, (0, -1)),
    (SW, (-1, -1))
    ]

shiftPoint (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

parseDirections [] = []
parseDirections ('e':line) = E:(parseDirections line)
parseDirections ('w':line) = W:(parseDirections line)
parseDirections ('n':'e':line) = NE:(parseDirections line)
parseDirections ('n':'w':line) = NW:(parseDirections line)
parseDirections ('s':'e':line) = SE:(parseDirections line)
parseDirections ('s':'w':line) = SW:(parseDirections line)

getPosition directions = foldl' (\i d -> shiftPoint i (vectors ! d)) (0, 0) directions

conwayLogic :: Bool -> Int -> Bool
conwayLogic True neighbourCount = if neighbourCount == 0 || neighbourCount > 2 then False else True
conwayLogic False neighbourCount = if neighbourCount == 2 then True else False

unique = S.toList . S.fromList

getNearbyCells :: S.Set (Int, Int) -> [(Int, Int)]
getNearbyCells m = unique [shiftPoint a n | a <- S.toList m, n <- (0, 0):(elems vectors)]

getNeighbourCount pos tiles = length (Prelude.filter (\i -> S.member i tiles) (map (shiftPoint pos) (elems vectors)))
nextStep tiles = S.fromList (Prelude.filter (\i -> conwayLogic (S.member i tiles) (getNeighbourCount i tiles)) (getNearbyCells tiles))

main = do
    contents <- readFile "inputs/24.txt"
    let flipPositions = map (getPosition . parseDirections) (lines contents)
    let currentTiles = S.fromList (keys (Data.Map.Strict.filter odd (foldl' (\m i -> insertWith (+) i 1 m) empty flipPositions)))
    let part1 = S.size currentTiles
    let part2 = S.size ((iterate nextStep currentTiles) !! 100)
    print (part1, part2)