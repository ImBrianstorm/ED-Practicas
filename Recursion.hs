factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib (n-2)

par :: Int -> Bool
par 0 = True
par n = impar(n-1)

impar :: Int -> Bool
impar 0 = False
impar n = par(n-1)

mult :: Int -> Int -> Int
mult _ 0 = 0
mult 0 _ = 0
mult n m =
    n + mult n(m-1)

factorialinv :: Int -> Int -> Int
factorialinv 0 m = 1
factorialinv n m = if n == m then n
                else m * factorialinv n (m+1)
