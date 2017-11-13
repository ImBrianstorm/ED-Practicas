data Exp = Num N | Suma Exp Exp | Prod Exp Exp | Paren Exp deriving (Eq,Show)
data N = Dig D | ConsN N D deriving (Eq,Show)
data D = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq,Show)

data TreeD = NumT Int | SumaT TreeD TreeD | ProdT TreeD TreeD | ParenT TreeD deriving (Eq,Show)

nToInt :: N -> Int
nToInt (Dig D0) = 0
nToInt (Dig D1) = 1
nToInt (Dig D2) = 2
nToInt (Dig D3) = 3
nToInt (Dig D4) = 4
nToInt (Dig D5) = 5
nToInt (Dig D6) = 6
nToInt (Dig D7) = 7
nToInt (Dig D8) = 8
nToInt (Dig D9) = 9
nToInt (ConsN x y) = read ((show (nToInt x)) ++ (show (nToInt (Dig y))))

intToN :: Int -> N
intToN 0 = (Dig D0)
intToN 1 = (Dig D1)
intToN 2 = (Dig D2)
intToN 3 = (Dig D3)
intToN 4 = (Dig D4)
intToN 5 = (Dig D5)
intToN 6 = (Dig D6)
intToN 7 = (Dig D7)
intToN 8 = (Dig D8)
intToN 9 = (Dig D9)
intToN n = ConsN (intToN (stringToInt (init (intToString n))))  (digToD (intToN (stringToInt [last (intToString n)])))

--Empiezan funciones auxiliares intToN

stringToInt :: String -> Int
stringToInt x = read x

intToString :: Int -> String
intToString n = show n

digToD :: N -> D
digToD (Dig x) = x

--Terminan funciones auxiliares intToN

expToTree :: Exp -> TreeD
expToTree (Num x) = NumT (nToInt x)
expToTree (Suma exp1 exp2) = SumaT (expToTree exp1) (expToTree exp2)
expToTree (Prod exp1 exp2) = ProdT (expToTree exp1) (expToTree exp2)
expToTree (Paren exp1) = ParenT (expToTree exp1)


treeToExp:: TreeD -> Exp
treeToExp (NumT x) = Num (intToN x)
treeToExp (SumaT tree1 tree2) = Suma (treeToExp tree1) (treeToExp tree2)
treeToExp (ProdT tree1 tree2) = Prod (treeToExp tree1) (treeToExp tree2)
treeToExp (ParenT tree) = Paren (treeToExp tree)

evalT :: TreeD -> Int
evalT (NumT x) = x
evalT (SumaT tree1 tree2) = (evalT tree1) + (evalT tree2)
evalT (ProdT tree1 tree2) = (evalT tree1) * (evalT tree2)
evalT (ParenT tree) = (((evalT tree)))

a = Prod (Num (ConsN (Dig D1) D0)) (Paren (Suma (Suma (Num (ConsN (ConsN (Dig D2) D5) D8)) (Num (ConsN (Dig D4) D7))) (Num (ConsN (ConsN (Dig D3) D6) D9))))
b = Suma (Num (ConsN (ConsN (ConsN (Dig D1) D0) D0) D0)) (Paren (Suma (Paren (Prod (Num (ConsN (ConsN (Dig D3) D0) D0)) (Num (Dig D2)))) (Num (ConsN (Dig D1) D0))))
c = Suma (Prod (Num (Dig D3)) (Num (ConsN (ConsN (Dig D6) D9) D9))) (Num (Dig D2))
d = Prod (Paren (Suma (Num (ConsN (ConsN (Dig D1) D5) D4)) (Num (ConsN (ConsN (ConsN (Dig D8) D0) D1) D6)))) (Paren (Suma (Num (ConsN (ConsN (Dig D2) D9) D9)) (Num (ConsN (ConsN (Dig D7) D3) D4))))
e = Prod (Paren (Suma (Num (ConsN (ConsN (Dig D5) D8) D4)) (Num (ConsN (ConsN (ConsN (Dig D3) D1) D1) D5)))) (Num (Dig D2))
f = Suma (Num (Dig D2)) (Num (Dig D2))

p1 = (evalT $ expToTree a) == 6740
p2 = (evalT $ expToTree b) == 1610
p3 = (evalT $ expToTree c) == 2099
p4 = (evalT $ expToTree d) == 8439610
p5 = (evalT $ expToTree e) == 7398
p6 = (evalT $ expToTree f) == 4