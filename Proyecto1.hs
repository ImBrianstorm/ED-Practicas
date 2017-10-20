data Natural = Cero | Suce Natural deriving Show

sumaN1 :: Natural -> Natural -> Natural
sumaN1 x Cero = x
sumaN1 Cero y = y
sumaN1 x y = 
