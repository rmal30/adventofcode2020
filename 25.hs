modPower :: Integer -> Integer -> Integer -> Integer
modPower _ 0 _ = 1
modPower base index modulus =
    if exponentRem == 0 then
        rem half modulus
    else
        rem (half * base) modulus
    where
        (exponentQuot, exponentRem) = quotRem index 2
        half = (modPower base exponentQuot modulus) ^ (2 :: Integer)

discreteLog :: Integer -> Integer -> Integer -> Maybe Integer
discreteLog base expected modulus = 
    if null sols then 
        Nothing 
    else 
        Just index
    where
        sols = filter (\(_, value) -> value == expected) (zip [0..(modulus - 1)] (iterate (\value -> rem (value * base) modulus) 1))
        (index, _):_ = sols

getEncryptionKey :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
getEncryptionKey subjectNumber cardPublicKey doorPublicKey modulus = do
    cardLoopSize <- discreteLog subjectNumber cardPublicKey modulus
    doorLoopSize <- discreteLog subjectNumber doorPublicKey modulus
    Just (modPower subjectNumber (cardLoopSize * doorLoopSize) modulus)

main :: IO ()
main = do
    contents <- readFile "inputs/25.txt"
    let cardPublicKey:doorPublicKey:_ = map read (lines contents)
    print (getEncryptionKey 7 cardPublicKey doorPublicKey 20201227)