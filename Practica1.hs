media3 :: (Float,Float,Float) -> Float
media3 (x,y,z) = (x + y + z) / 3

absoluto :: Int -> Int
absoluto x = if x > 0 then x else -1*x

esTriangulo :: (Num a, Ord a) => a -> a -> a -> Bool
esTriangulo x y z =  x+y>z && x+z>y && y+z>x

maxRect :: (Float,Float) -> (Float,Float) -> (Float,Float)
maxRect (b1,h1) (b2,h2) = if (b1*h1)>(b2*h2) then (b1,h1) else (b2,h2)

dPuntos :: (Float,Float) -> (Float,Float) -> Float
dPuntos (x1,y1) (x2,y2) = sqrt( ((x2-x1)^2) + ((y2-y1)^2) )

sumaC :: (Double,Double) -> (Double,Double) -> (Double,Double)
sumaC (a,b) (c,d) = (a+c,b+d)

restaC :: (Double,Double) -> (Double,Double) -> (Double,Double)
restaC (a,b) (c,d) = (a-c,b-d)

multC :: (Double,Double) -> (Double,Double) -> (Double,Double)
multC (a,b) (c,d) = (a*c-b*d, a*d+b*c)

divC :: (Double,Double) -> (Double,Double) -> (Double,Double)
divC (a,b) (c,d) = ((a*c+b*d)/(c^2+d^2), (b*c-a*d)/(c^2+d^2))

conjugado :: (Double,Double) -> (Double,Double)
conjugado (a,b) = (a,-b)

realC :: (Double,Double) -> Double
realC (a,b) = a

imaginarioC :: (Double,Double) -> Double
imaginarioC (a,b) = b
