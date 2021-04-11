modPower :: Integer -> Integer -> Integer -> Integer
modPower a 0 c = 1
modPower a b c =
    if mod b 2 == 0 then
        mod u c
    else
        mod (a*u) c
    where
        u = (modPower a (div b 2) c) ^ 2

discreteLog :: Integer -> Integer -> Integer -> Maybe Integer
discreteLog b a m = if null sols then Nothing else Just count
    where
        sols = dropWhile (\(i, v) -> v /= a) (zip [0..(m - 1)] (iterate (\v -> rem (v * b) m) 1))
        (count, _):_ = sols

solve :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
solve subjectNumber cardPublicKey doorPublicKey modulus = do
    cardLoopSize <- discreteLog subjectNumber cardPublicKey modulus
    doorLoopSize <- discreteLog subjectNumber doorPublicKey modulus
    Just (modPower subjectNumber (cardLoopSize * doorLoopSize) modulus)

main = do
    contents <- readFile "inputs/25.txt"
    let cardPublicKey:doorPublicKey:_ = map read (lines contents)
    print (solve 7 cardPublicKey doorPublicKey 20201227)