import qualified Data.Set as S
import qualified Data.Map as M
import Data.List(foldl')
import Utils(join, split)

parseFood foodStr = (split ' ' ingredientsStr, map (dropWhile (==' ')) foods )
    where
        [ingredientsStr, allergensStr] = split '(' foodStr
        foods = split ',' (drop 9 (init allergensStr))

addMatch m ingredients allergens = foldl' (\m2 allergen ->
        if M.member allergen m then
            M.insertWith (S.intersection) allergen ingredientSet m2
        else
            M.insert allergen ingredientSet m2
            ) m allergens
    where
        ingredientSet = S.fromList ingredients

getCandidates foods = foldl' (\m (ingredients, allergens) -> addMatch m ingredients allergens) M.empty foods

exclude s x = foldl' (\m i -> S.delete i m) s x

simplifyMatches (m, x) = (M.insert k value m, newX)
    where
        (k, s) = head (filter (\(a, b) -> S.size b == 1) (M.toList x))
        value = head (S.toList s)
        newX = M.map (\mv -> S.delete value mv) (M.delete k x)

main = do
    contents <- readFile "inputs/21.txt"
    let foodStrs = lines contents
    let foods = map parseFood foodStrs
    let allergens = S.fromList (concat (map snd foods))
    let matches = getCandidates foods
    let possibleAllergicIngredients = foldl' S.union S.empty (M.elems matches)
    let part1 = sum (map (\(i, _) -> S.size (exclude (S.fromList i) possibleAllergicIngredients)) foods)
    let part2 = join ',' (M.elems (fst (head (dropWhile (\(_, i) -> not (M.null i)) (iterate simplifyMatches (M.empty, matches))))))
    print (part1, part2)