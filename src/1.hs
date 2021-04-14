import qualified Data.Set as S

getPairsWithSum :: Integral a => S.Set a -> a -> [(a, a)]
getPairsWithSum set target = [(element1, element2) | element1 <- S.elems set, let element2 = target - element1, S.member element2 set]

getTriplesWithSum :: Integral a => S.Set a -> a -> [(a, a, a)]
getTriplesWithSum set target = [(element1, element2, element3) | 
        element1 <- S.elems set, element2 <- S.elems set, 
        let element3 = target - element1 - element2, 
        S.member element3 set
    ]

main :: IO ()
main = do
    contents <- readFile "inputs/1.txt"
    let set = S.fromList (map read (lines contents))
    let target = 2020 :: Int
    let (p1, p2):_ = set `getPairsWithSum` target
    let (t1, t2, t3):_ = set `getTriplesWithSum` target
    print (p1 * p2, t1 * t2 * t3)