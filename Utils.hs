module Utils(split, join, takeWhileChange) where
    split :: Eq a => a -> [a] -> [[a]]
    _ `split` [] = []
    separator `split` items = prefix:(
        if not (null remaining) then 
            separator `split` suffix
        else
            []
        )
        where
            (prefix, remaining) = break (==separator) items
            _:suffix = remaining

    join :: a -> [[a]] -> [a]
    _ `join` [] = []
    separator `join` (x:xs)  = x ++ (if null xs then [] else (separator:(separator `join` xs)))

    takeWhileChange [] = []
    takeWhileChange [x] = [x]
    takeWhileChange (x:y:xs) = if x == y then [x] else x:takeWhileChange (y:xs)
