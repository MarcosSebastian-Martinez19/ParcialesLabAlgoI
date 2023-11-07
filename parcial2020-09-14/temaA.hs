-- Ejercicio 1
estaEnDni :: Int -> Bool
estaEnDni 4 = True
estaEnDni 6 = True
estaEnDni 7 = True
estaEnDni 2 = True
estaEnDni 3 = True
estaEnDni 8 = True
estaEnDni _ = False

-- Ejercicio 2

sumaDNI :: [Int] -> Int
sumaDNI [] = 0
sumaDNI (x:xs)
    | estaEnDni x = x + sumaDNI xs
    | otherwise = sumaDNI xs

-- Ejercicio 3

digitoASumar :: Int -> Int
digitoASumar x
    | estaEnDni x = x
    | otherwise = 0

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + (sumatoria' xs f)

sumaDNI' :: [Int] -> Int
sumaDNI' xs = sumatoria' xs (digitoASumar)

-- Ejercicio 4
reducir :: [a] -> (a -> a -> a) ->a
reducir [x] _ = x
reducir (x:xs) f = x `f` reducir xs f
