factorial :: Int -> Int -> Int
factorial 0 m = 1
factorial n m =
        if n == m then n
        else m * factorial n (m+1)
