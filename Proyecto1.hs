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


data Arbol = Hoja Int | Nodo Arbol Int Arbol deriving Show

aparece :: Int -> Arbol -> Bool
aparece x (Hoja y) =  if x==y then True else False
aparece x (Nodo izq y der) = if aparece x izq || aparece x der then True else False

aplanaPO :: Arbol -> [Int]
aplanaPO (Hoja x) = [x]
aplanaPO (Nodo izq x der) = x:(aplanaPO izq ++ aplanaPO der)

aplanaIO :: Arbol -> [Int]
aplanaIO (Hoja x) = [x]
aplanaIO (Nodo izq x der) = (aplanaIO izq) ++   x:(aplanaIO der)

aplanaPsO :: Arbol -> [Int]
aplanaPsO (Hoja x) = [x]
aplanaPsO (Nodo izq x der) = (aplanaPsO izq ++ aplanaPsO der) ++  [x]


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


type ProductoBI = [(Int,Int)]
type RelacionBI = ProductoBI

union :: RelacionBI -> RelacionBI-> RelacionBI
union x [] = x
union x (y:ys) = if y `elem` x then union x ys else union x ys ++ [y]

interseccion :: RelacionBI -> RelacionBI-> RelacionBI
interseccion x [] = []
interseccion x (y:ys) = if y `elem` x then interseccion x ys ++ [y] else interseccion x ys

diferencia :: RelacionBI -> RelacionBI-> RelacionBI
diferencia x [] = x
diferencia [] y = []
diferencia (x:xs) y = if x `elem` y then diferencia xs y  else [x] ++ diferencia xs y

inversa :: RelacionBI -> RelacionBI
inversa [(x,y)] = [(y,x)]
inversa ((x,y):xs) = [(y,x)] ++ inversa xs

composicion :: RelacionBI -> RelacionBI -> RelacionBI
composicion x [] = []
composicion [] (y:ys) = []
composicion [(x1,y1)] [(x2,y2)] = if y1==x2 then [(x1,y2)] else []  
composicion ((x1,y1):xs) ((x2,y2):ys) = (if y1/=x2 then composicion ((x1,y1):xs) ys else [(x1,y2)] ++ composicion ((x1,y1):xs) ys) ++ composicion xs ((x2,y2):ys)

reflexividad :: RelacionBI -> Bool
reflexividad [] = False
reflexividad [(x,y)] = if x==y then True else False
reflexividad ((x,y):xs) =  if ((x,x)`elem`((x,y):xs)) && ((y,y)`elem`((x,y):xs)) then reflexividad (xs) else False