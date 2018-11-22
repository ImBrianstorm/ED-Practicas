
factorial n = factorial_cola n 1

factorial_cola 0 m = m
factorial_cola n m = (factorial_cola (n-1) (n*m))