import qualified Data.Set as S
import qualified Data.Map as M
import Data.List(foldl')
import Utils(join, split)

parseFood :: String -> ([String], [String])
parseFood foodStr = (split ' ' ingredientsStr, map (dropWhile (==' ')) foods)
    where
        [ingredientsStr, allergensStr] = split '(' foodStr
        foods = split ',' (drop 9 (init allergensStr))

addMatch :: M.Map String (S.Set String) -> [String] -> [String] -> M.Map String (S.Set String)
addMatch possibleAllergensMap ingredients = foldl' (\possibleAllergensMap' allergen ->
        if M.member allergen possibleAllergensMap then
            M.insertWith S.intersection allergen ingredientSet possibleAllergensMap'
        else
            M.insert allergen ingredientSet possibleAllergensMap'
            ) possibleAllergensMap
    where
        ingredientSet = S.fromList ingredients

getCandidates :: [([String], [String])] -> M.Map String (S.Set String)
getCandidates = foldl' (\possibleAllergensMap (ingredients, allergens) -> addMatch possibleAllergensMap ingredients allergens) M.empty

exclude :: Ord a => S.Set a -> S.Set a -> S.Set a
exclude = foldl' (flip S.delete)

simplifyMatches :: (M.Map String String, M.Map String (S.Set String)) -> (M.Map String String, M.Map String (S.Set String))
simplifyMatches (matches, possibleMatches) = (M.insert allergen allergicIngredient matches, newPossibleMatches)
    where
        (allergen, possibleIngredient) = head (filter (\(_, possibleIngredients) -> S.size possibleIngredients == 1) (M.toList possibleMatches))
        allergicIngredient:_ = S.toList possibleIngredient
        newPossibleMatches = M.map (S.delete allergicIngredient) (M.delete allergen possibleMatches)

main :: IO ()
main = do
    contents <- readFile "inputs/21.txt"
    let foodStrs = lines contents
    let foods = map parseFood foodStrs
    let possibleMatches = getCandidates foods
    let possibleAllergicIngredients = foldl' S.union S.empty (M.elems possibleMatches)
    let part1 = sum [S.size (exclude (S.fromList ingredients) possibleAllergicIngredients) | (ingredients, _) <- foods]
    let (allergenIngredientMap, _):_ = dropWhile (\(_, possibleMatches') -> not (M.null possibleMatches')) (iterate simplifyMatches (M.empty, possibleMatches))
    let part2 = join ',' (M.elems allergenIngredientMap)
    print (part1, part2)