import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (foldl')
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

union x = foldl' S.union S.empty x


join _ [] = []
join tok (x:arr) = x ++ (if null arr then [] else ([tok] ++ join tok arr))

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (==' ')

parseDependency :: String -> (String, Int)
parseDependency str = (join '_' (init item), read n) where (n:item) = split ' ' (trim str)

parseRule str = (goal, dependencies)
    where
        [rawGoal, rawDependencies] = split "contain" (split ' ' str)
        goal = join '_' (init rawGoal)
        dependenciesStr = init (join ' ' rawDependencies)
        dependencies = parseDependencies dependenciesStr
        parseDependencies "no other bags" = []
        parseDependencies x = map parseDependency (split ',' (trim x))


addEntry goal m (d, _) =
            case M.lookup d m of
                Just goals -> M.insert d (S.insert goal goals) m
                Nothing -> M.insert d (S.fromList [goal]) m


addReverseDependencies m (goal, dependencies) = foldl' (addEntry goal) m dependencies

getNeighbours tree items = S.toList (union ((S.fromList items):[
    case M.lookup item tree of
        Just s -> s
        Nothing -> S.empty
    | item <- items]))

takeWhileChange [] = []
takeWhileChange [x] = [x]
takeWhileChange [x,y] = if x == y then [x] else [x,y]
takeWhileChange (x:y:xs) = if x == y then [x] else x:takeWhileChange (y:xs)

countDependencies goal tree =
    case M.lookup goal tree of
        Just m -> sum [v*(1 + countDependencies k tree) | (k,v) <- M.toList m]
        Nothing -> 0


main = do
    contents <- readFile "inputs/7.txt"
    let rules = map parseRule (lines contents)
    let dependencyTree = M.fromList [(k, M.fromList v) | (k, v) <- rules]
    let reverseTree = foldl' addReverseDependencies M.empty rules
    let part1 = last (takeWhileChange (map length (iterate (getNeighbours reverseTree) ["shiny_gold"]))) - 1
    let part2 = countDependencies "shiny_gold" dependencyTree
    print (part1, part2)