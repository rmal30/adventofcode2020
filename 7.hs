import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (foldl')
import Utils(split, join, takeWhileChange)

trim :: String -> String
trim = removeLeadingAndReverse . removeLeadingAndReverse
    where 
        removeLeadingAndReverse = reverse . dropWhile (==' ')

parseDependency :: String -> (String, Int)
parseDependency dependencyStr = (join '_' (init item), read count) 
    where 
        (count:item) = split ' ' (trim dependencyStr)

parseRule :: String -> (String, [(String, Int)])
parseRule ruleStr = (goal, dependencies)
    where
        [rawGoal, rawDependencies] = split "contain" (split ' ' ruleStr)
        goal = join '_' (init rawGoal)
        dependenciesStr = init (join ' ' rawDependencies)
        dependencies = parseDependencies dependenciesStr
        parseDependencies "no other bags" = []
        parseDependencies depsStr = map parseDependency (split ',' (trim depsStr))

addEntry :: (Ord k, Ord a) => a -> M.Map k (S.Set a) -> (k, b) -> M.Map k (S.Set a)
addEntry goal reverseDependenciesMap (dep, _) =
    case M.lookup dep reverseDependenciesMap of
        Just goals -> M.insert dep (S.insert goal goals) reverseDependenciesMap
        Nothing -> M.insert dep (S.fromList [goal]) reverseDependenciesMap

addReverseDependencies :: (Ord k, Ord a) => M.Map k (S.Set a) -> (a, [(k, b)]) -> M.Map k (S.Set a)
addReverseDependencies reverseDependenciesMap (goal, dependencies) = foldl' (addEntry goal) reverseDependenciesMap dependencies

getNeighbours :: Ord a => M.Map a (S.Set a) -> [a] -> [a]
getNeighbours tree items = S.toList (foldl' S.union S.empty (S.fromList items : [M.findWithDefault S.empty item tree | item <- items]))

countDependencies :: (Ord t, Num a) => t -> M.Map t (M.Map t a) -> a
countDependencies goal tree =
    case M.lookup goal tree of
        Just dependencies -> sum [count * (1 + countDependencies dep tree) | (dep, count) <- M.toList dependencies]
        Nothing -> 0

main :: IO ()
main = do
    contents <- readFile "inputs/7.txt"
    let target = "shiny_gold"
    let rules = map parseRule (lines contents)
    let dependencyTree = M.fromList [(goal, M.fromList deps) | (goal, deps) <- rules]
    let reverseTree = foldl' addReverseDependencies M.empty rules
    let part1 = last (takeWhileChange (map length (iterate (getNeighbours reverseTree) [target]))) - 1
    let part2 = countDependencies target dependencyTree
    print (part1, part2)