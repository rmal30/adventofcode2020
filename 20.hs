import Data.Array.IArray
import qualified Data.Map as M
import Data.List(foldl')
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split tok arr =
    if not (null y) then
        x:(split tok newArr)
    else
        [x]
    where
        (x, y) = break (==tok) arr
        newArr = tail y


data Color = Black | White deriving (Show, Eq)

parseTile :: [String] -> (Int, Array (Int, Int) Color)
parseTile (x:xs) = (tileNo, image)
    where
        [_, tileNoStr] = split ' ' x
        tileNo = read (init tileNoStr)
        tileSize = length xs
        image = array ((0, 0), (tileSize - 1, tileSize - 1)) [((x, y), if c == '#' then Black else White) | (y, row) <- zip [0..] xs, (x, c) <- zip [0..] row]

getEdges image = [
    [image ! (0, i) | i <- [0..my]],
    [image ! (mx, i) | i <- [my,(my - 1)..0]],
    [image ! (i, 0) | i <- [mx,(mx - 1)..0]],
    [image ! (i, my) | i <- [0..mx]]
    ]
    where
        (_, (mx,my)) = bounds image

flipImage image = ixmap b (\(i, j) -> (i, maxSize - j)) image
    where
        b = bounds image
        (_, (maxSize, _)) = b

getRotations image = [
        ixmap b (\(i, j) -> (i, j)) image,
        ixmap b (\(i, j) -> (maxSize - i, maxSize - j)) image,
        ixmap b (\(i, j) -> (maxSize - j, i)) image,
        ixmap b (\(i, j) -> (j, maxSize - i)) image
    ]
    where
        b = bounds image
        (_, (maxSize, _)) = b

getRotationsAndFlips image = getRotations image ++ getRotations (flipImage image)

placePieces :: M.Map Int (Array (Int, Int) Color) -> M.Map Int [Int] -> M.Map Int ((Int, Int), Array (Int, Int) Color) -> M.Map Int ((Int, Int), Array (Int, Int) Color)
placePieces tileMap neighbourMap assembledPieces = foldl' (\m (k, v) -> M.insert k v m) assembledPieces [
    (neighbourId, (newCoordinates, rotatedImage)) |
        (pieceId, ((x, y), image)) <- M.toList assembledPieces,
        let [leftImageEdge, rightImageEdge, upImageEdge, downImageEdge] = getEdges image,
        neighbourId <- neighbourMap M.! pieceId,
        not (M.member neighbourId assembledPieces),
        rotatedImage <- getRotationsAndFlips (tileMap M.! neighbourId),
        let
            [leftNeighbourEdge, rightNeighbourEdge, upNeighbourEdge, downNeighbourEdge] = getEdges rotatedImage
            condLeft = leftImageEdge == reverse rightNeighbourEdge
            condRight = rightImageEdge == reverse leftNeighbourEdge
            condUp = upImageEdge == reverse downNeighbourEdge
            condDown = downImageEdge == reverse upNeighbourEdge,
        newCoordinates <- map snd (filter fst (zip [condLeft, condRight, condUp, condDown] [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]))
    ]

trimBorders :: Array (Int, Int) a -> Array (Int, Int) a
trimBorders x = array ((0, 0), (a - 2, b - 2)) [((i - 1, j - 1), x ! (i, j)) | i <- [1..(a - 1)], j <- [1..(b - 1)]]
    where
        (_, (a, b)) = bounds x

mergeImage :: [((Int, Int), Array (Int, Int) a)] -> Array (Int, Int) a
mergeImage x = array ((0, 0), (dX*tileSize - 1, dY*tileSize - 1)) [(((x - minX)*tileSize  + i, (y - minY)*tileSize + j), image ! (i, j)) | ((x, y), image) <- x, (i, j) <- indices image]
    where
        (xCoords, yCoords) = unzip (map fst x)
        minX = minimum xCoords
        maxX = maximum xCoords
        minY = minimum yCoords
        maxY = maximum yCoords
        dX = maxX - minX + 1
        dY = maxY - minY + 1
        (_, (maxTileX, _)) = bounds (snd (head x))
        tileSize = 1 + maxTileX

seaMonster = ["                  #", "#    ##    ##    ###", " #  #  #  #  #  #   "]

getCoordinates patternStr = [(i, j) | (i, row) <- zip [0..] patternStr, (j, _) <- filter (\(_, c) -> c == '#') (zip [0..] row)]

checkPatternAtPosition :: Array (Int, Int) Color -> [(Int, Int)] -> (Int, Int) -> Bool
checkPatternAtPosition image pattern (x, y) = all (\(i, j) -> image ! (x + i, y + j) == Black) pattern

countPatterns pattern image  = length (filter (checkPatternAtPosition image pattern) [(i, j) | i <- [lx..(hx - px)], j <- [ly..(hy - py)]])
    where
        ((lx, ly), (hx, hy)) = bounds image
        (px, py) = (maximum (map fst pattern), maximum (map snd pattern))

main = do
    contents <- readFile "inputs/20.txt"
    let tiles = map parseTile (split "" (lines contents))
    let tileMap = M.fromList tiles
    let imageEdgeMap = M.fromList (map (\(no, image) -> (no, getEdges image)) tiles)
    let edges = [(n1, n2) | (n1, b1) <- M.toList imageEdgeMap, (n2, b2) <- M.toList imageEdgeMap, e1 <- b1, e2 <- b2, n1 < n2, e1 == e2 || reverse e1 == e2]
    let neighbourMap = foldl' (\x (n1, n2) -> M.insertWith (++) n2 [n1] (M.insertWith (++) n1 [n2] x)) M.empty edges

    let cornerIds = M.keys (M.filter (\v -> length v == 2) neighbourMap)
    let part1 = product cornerIds

    let firstCorner = head cornerIds
    let assembledPieces = head (dropWhile (\i -> length i < M.size tileMap) (iterate (placePieces tileMap neighbourMap) (M.fromList [(firstCorner, ((0, 0), tileMap M.! firstCorner))])))
    let assembledImage = mergeImage (map (\((x, y), i) -> ((x, y), trimBorders i)) (M.elems assembledPieces))
    let pattern = getCoordinates seaMonster
    let allImages = getRotationsAndFlips assembledImage
    let countBlack = length (filter (==Black) (elems assembledImage))
    let part2 = countBlack - (length pattern)*(head (filter (>0) (map (countPatterns pattern) allImages)))
    print (part1, part2)