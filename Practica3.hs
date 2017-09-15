sumaR :: Int -> Int -> Int
sumaR n 0 = n
sumaR 0 m = m
sumaR n m = succ(sumaR n (m-1))

restaR :: (Eq a, Num a) => a -> a -> a
restaR x 0 = x
restaR 0 y = -y
restaR x y = restaR (x-1) (y-1)

menorQ :: Int -> Int -> Bool
menorQ x 0 = False
menorQ 0 y = True
menorQ x y = menorQ (x-1) (y-1)

mayorQ :: Int -> Int -> Bool
mayorQ x 0 = True
mayorQ 0 y = False
mayorQ x y = mayorQ (x-1) (y-1)

igualQ :: Int -> Int -> Bool
igualQ 0 0 = True
igualQ x 0 = False
igualQ 0 y = False
igualQ x y = igualQ (x-1) (y-1)

power :: Int -> Int -> Int
power n 0 = 1
power n m = n * power n (m-1)

divInt :: Int -> Int -> Int
divInt x y = if x < y then 0
             else if x == y then 1
             else 1 + divInt (x-y) y

power2 :: Int -> Int -> Int
power2 n 2 = power n 2
power2 n k = if (k`mod`2)==0 then  power n 2 * power2 n (k`div`2)
             else n * power2 n (k-1)

dobleFactorial :: Int -> Int
dobleFactorial 2 = 2
dobleFactorial 1 = 1
dobleFactorial n = n * dobleFactorial (n-2)

pertenece :: Eq a => a -> [a] -> Bool
pertenece x (y:ys) = if (length (y:ys) ) > 1 && x == y then True
                     else if length (y:ys) == 1 && x == y then True
                     else if length (y:ys) == 1 && x /= y then False
                     else pertenece x (drop 1 (y:ys))



sumaP :: (Eq a, Num a, Ord a) => a -> a -> a
sumaP x y = if y < 0 then x + y
            else if y == 0 then x
            else 1 + sumaP x (y-1)
