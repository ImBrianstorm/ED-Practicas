data Natural = Cero | Suce Natural deriving Show

sumaN1 :: Natural -> Natural -> Natural
sumaN1 x Cero = x
sumaN1 Cero y = y
sumaN1 x (Suce y) = Suce (sumaN1 x y)

multN1 :: Natural -> Natural -> Natural
multN1 x Cero = Cero
multN1 Cero y = y
multN1 x (Suce Cero) = x 
multN1 (Suce Cero) y = y
multN1 x (Suce y) = sumaN1 (multN1 x y) (x)

natToInt :: Natural -> Int
natToInt Cero = 0
natToInt (Suce x) = 1 + natToInt x

intToNat :: Int -> Natural
intToNat 0 = Cero
intToNat x = Suce(intToNat (x-1))

restaSeg :: Natural -> Natural -> Natural
restaSeg x Cero = x
restaSeg Cero x = error "No definido"
restaSeg (Suce x) (Suce y) = restaSeg x y


data Lista = Vacia | Conc Int Lista deriving Show

long :: Lista -> Int
long Vacia = 0
long (Conc n Vacia) = 1
long (Conc n (Conc n2 x)) = 1 + long (Conc n2 x)

takeL :: Int -> Lista -> Lista
takeL 0 x = x
takeL 1 (Conc int x) = Conc int Vacia
takeL n (Conc int (Conc int2 x)) = Conc int (takeL (n-1) (Conc int2 x))

dropL :: Int -> Lista -> Lista
dropL 0 x = x
dropL n (Conc int (Conc int2 x)) = dropL (n-1) (Conc int2 x)

intAt1 :: Lista -> Int
intAt1 Vacia = error "Lista Vacía"
intAt1 (Conc int1 x) = int1 

concatena :: Lista -> Lista -> Lista
concatena x Vacia = x
concatena (Conc int Vacia) y = Conc int y
concatena x y = concatena (takeL ((long x)-1) x) (Conc (intAt1 (dropL ((long x)-1) x)) y)

reversa :: Lista -> Lista
reversa Vacia = Vacia
reversa (Conc int Vacia) = Conc int Vacia
reversa x = concatena (dropL ((long x)-1) x) (reversa (takeL ((long x)-1) x))

data Arbol = Hoja Int | Nodo Arbol Int Arbol deriving Show

aparece :: Int -> Arbol -> Bool