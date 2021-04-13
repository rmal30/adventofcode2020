import qualified Data.Set as S
import qualified Data.Map as M
import Data.List(foldl')
import Utils(join, split)

parseFood :: String -> ([String], [String])
parseFood foodStr = (split ' ' ingredientsStr, map (dropWhile (==' ')) foods )
    where
        [ingredientsStr, allergensStr] = split '(' foodStr
        foods = split ',' (drop 9 (init allergensStr))

addMatch :: M.Map String (S.Set String) -> [String] -> [String] -> M.Map String (S.Set String)
addMatch m ingredients = foldl' (\m2 allergen ->
        if M.member allergen m then
            M.insertWith S.intersection allergen ingredientSet m2
        else
            M.insert allergen ingredientSet m2
            ) m
    where
        ingredientSet = S.fromList ingredients

getCandidates :: [([String], [String])] -> M.Map String (S.Set String)
getCandidates = foldl' (\m (ingredients, allergens) -> addMatch m ingredients allergens) M.empty

exclude :: Ord a => S.Set a -> S.Set a -> S.Set a
exclude = foldl' (flip S.delete)

simplifyMatches :: (M.Map String String, M.Map String (S.Set String)) -> (M.Map String String, M.Map String (S.Set String))
simplifyMatches (m, x) = (M.insert k value m, newX)
    where
        (k, s) = head (filter (\(_, b) -> S.size b == 1) (M.toList x))
        value = head (S.toList s)
        newX = M.map (S.delete value) (M.delete k x)

main :: IO ()
main = do
    contents <- readFile "inputs/21.txt"
    let foodStrs = lines contents
    let foods = map parseFood foodStrs
    let matches = getCandidates foods
    let possibleAllergicIngredients = foldl' S.union S.empty (M.elems matches)
    let part1 = sum [S.size (exclude (S.fromList i) possibleAllergicIngredients) | (i, _) <- foods]
    let part2 = join ',' (M.elems (fst (head (dropWhile (\(_, i) -> not (M.null i)) (iterate simplifyMatches (M.empty, matches))))))
    print (part1, part2)