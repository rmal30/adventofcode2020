import qualified Data.Set as S
import Utils(split)

iterateWhileJust :: (a -> Maybe a) -> a -> [a]
iterateWhileJust func initial = initial : 
    case func initial of
        Just value -> iterateWhileJust func value
        Nothing -> []

type GameState = ([Int], [Int])

giveCardsToWinner :: GameState -> Bool -> Maybe GameState
giveCardsToWinner ([], _) _ = Nothing
giveCardsToWinner (_, []) _ = Nothing
giveCardsToWinner (top1:deck1, top2:deck2) firstWins =
    if firstWins then
        Just (deck1 ++ [top1, top2], deck2)
    else
        Just (deck1, deck2 ++ [top2, top1])

playRound :: GameState -> Maybe GameState
playRound ([], _) = Nothing
playRound (_, []) = Nothing
playRound (top1:deck1, top2:deck2) = giveCardsToWinner (top1:deck1, top2:deck2) (top1 > top2)

playRecursiveRound :: (S.Set GameState, GameState) -> Maybe (S.Set GameState, GameState)
playRecursiveRound (_, ([], _)) = Nothing
playRecursiveRound (_, (_, [])) = Nothing
playRecursiveRound (history, (top1:deck1, top2:deck2)) 
        | S.member (top1:deck1, top2:deck2) history = Nothing
        | otherwise = do
            let newHistory = S.insert (top1:deck1, top2:deck2) history
            let (winner, _) = playRecursiveCombat (take top1 deck1, take top2 deck2)
            let firstWins = if top1 <= length deck1 && top2 <= length deck2 then winner == 1 else top1 > top2
            newCards <- giveCardsToWinner (top1:deck1, top2:deck2) firstWins
            return (newHistory, newCards)
        
getScore :: [Int] -> Int
getScore deck = sum (zipWith (*) (reverse deck) [1..])

playCombat :: ([Int], [Int]) -> (Int, Int)
playCombat (deck1, deck2) =
        if null newDeck1 then
            (2, getScore newDeck2)
        else
            (1, getScore newDeck1)
    where
        (newDeck1, newDeck2) = last (iterateWhileJust playRound (deck1, deck2))

playRecursiveCombat :: ([Int], [Int]) -> (Int, Int)
playRecursiveCombat (deck1, deck2) =
        if null newDeck2 || S.member (newDeck1, newDeck2) history then
            (1, getScore newDeck1)
        else
            (2, getScore newDeck2)
    where
        (history, (newDeck1, newDeck2)) = last (iterateWhileJust playRecursiveRound (S.empty, (deck1, deck2)))

main :: IO ()
main = do
    contents <- readFile "inputs/22.txt"
    let [_:deck1Str, _:deck2Str] = "" `split` lines contents
    let (deck1, deck2) = (map read deck1Str, map read deck2Str)
    let (_, part1) = playCombat (deck1, deck2)
    let (_, part2) = playRecursiveCombat (deck1, deck2)
    print (part1, part2)