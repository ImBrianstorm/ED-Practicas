{------------------------------
           PRACTICA 1
      Mauricio Chávez Olea
   Fecha de entrega: 25/10/18
--------------------------------}

main :: IO ()
main = return ()

data Nat = Cero | Suc Nat deriving (Show)

toNat :: Int -> Nat
toNat 0 = Cero
toNat n = Suc (toNat (n-1))

toInt :: Nat -> Int
toInt Cero = 0
toInt (Suc n) = toInt n + 1

factorial :: Nat -> Nat
factorial Cero = Suc Cero
factorial (Suc n) = mult (Suc n) (factorial n)

potencia :: Nat -> Nat -> Nat
potencia _ Cero = Suc Cero
potencia n (Suc m) = mult n (potencia n m)


-- AUXILIARES

suma :: Nat -> Nat -> Nat
suma n Cero = n
suma n (Suc m) = Suc (suma n m)

mult :: Nat -> Nat -> Nat
mult _ Cero = Cero
mult n (Suc m) = suma (mult n m) n