replica :: Int -> Int -> [Int]
replica _ 0 = []
replica x n = x:(replica x (n-1))

rota :: [Int] -> Int -> [Int]
rota xs 0 = xs
rota (x:xs) n = (rota (xs ++ [x]) (n-1))

data Tree = Void | T Tree Int Tree deriving (Show)

nInternos :: Tree -> Int
nInternos Void = 0
nInternos (T Void _ Void) = 0
nInternos (T t1 n t2) = 1 + (nInternos t1) + (nInternos t2)

sumaT :: Tree -> Tree -> Tree
sumaT Void t = t
sumaT t Void = t
sumaT (T t1 n t2) (T t3 m t4) = (T (sumaT t1 t3) (n+m) (sumaT t2 t4))

toTree :: [Int] -> Tree
toTree [] = Void
toTree (x:xs) = T (toTree xs) x (toTree xs)