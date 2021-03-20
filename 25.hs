modPower :: Integer -> Integer -> Integer -> Integer
modPower a 0 c = 1
modPower a b c =
    if mod b 2 == 0 then
        mod u c
    else
        mod (a*u) c
    where
        u = (modPower a (div b 2) c) ^ 2

key = 20201227

main = print (modPower 7 ((8516637 + 1)*(11710224 + 1)) key)