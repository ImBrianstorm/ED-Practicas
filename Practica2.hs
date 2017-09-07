gtaveragethree :: Int -> Int -> Int -> Int
gtaveragethree x y z  =
                      if ((x+y+z) `div` 3)<x then x
                      else if ((x+y+z) `div` 3)<y then y
                      else z

palindromo :: String -> Bool
palindromo [] = error "No definido"
palindromo x = if x == reverse x then True
                          else False

minMax :: Ord a => [a] -> (a,a)
minMax x = (maximum x, minimum x )

atN :: [a] -> Int -> a
atN x n = head (drop (n-1) (take n x))

selectMin :: [Int] -> [Int]
selectMin x = drop ((minimum x)-1) (take (minimum x) x)

deleteMin :: [Int] -> [Int]
deleteMin x = drop (minimum x) x

delN :: Int -> [a] -> [a]
delN n [] = error "No definido"
delN n x = take (n-1) x ++ drop n x

intervalos :: Int -> Int -> [a] -> [a]
intervalos x y [] = error "No definido"
intervalos n1 n2 x =  drop (n1 - 1) (take n2 x)

dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos n1 n2 [] = error "No definido"
dIntervalos n1 n2 x = take (n1-1) x ++ drop n2 x

avgLen :: [Int] -> Bool
avgLen x = if (sum x) `div` (length x) <= length x then True
                                                   else False
