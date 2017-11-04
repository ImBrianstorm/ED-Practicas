import Data.Char

primeros :: [(a,b)] -> [a]
primeros ls = [fst par | par <- ls]

segundos :: [(a,b)] -> [b]
segundos ls = [snd par | par <- ls]

divN :: Int -> [Int]
divN 0 = error "No es posible dividir entre cero"
divN n = [int | int <- [1..100], int`mod`n == 0]

minusculas :: String -> String
minusculas str = [toLower c | c <- str]

mPares :: Num a => [(a,a)] -> [a]
mPares ls = [fst par * snd par | par <- ls]

sumaCuadrados :: Int -> Int
sumaCuadrados n = sum [int^2 | int <- [n,(n-1)..0]]

desdobla :: [[a]] -> [a]
desdobla [] = []
desdobla ls = if length ls > 1 then [x | x <- head ls] ++ desdobla (tail ls) else head ls

prodC :: [a] -> [b] -> [(a,b)]
prodC l1 l2 = [(x,y) | x <- l1 , y <- l2]

separa :: [(a,b)] -> ([a],[b])
separa ls = ([fst x | x <- ls],[snd x | x <- ls])

primos :: Int -> [Int]
primos n = [numero | numero <- [2,3..n], esPrimo numero]

--Funciones auxiliares

esPrimo :: Int -> Bool
esPrimo n = ([divisor | divisor <- [1,2..n], (n`mod`divisor) == 0]) == [1,n]

--Funciones extra

primos' :: Int -> [Int]
primos' n = [numero | numero <- [2,3..n], ([divisor | divisor <- [1,2..n], (numero`mod`divisor) == 0]) == [1,numero]]

separa' :: [(a,b)] -> ([a],[b])
separa' ls = (primeros ls,segundos ls)