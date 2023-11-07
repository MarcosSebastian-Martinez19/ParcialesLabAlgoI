-- Ejercicio 1
estaEnDNI :: Int -> Bool
estaEnDNI 4 = True
estaEnDNI 6 = True
estaEnDNI 7 = True
estaEnDNI 2 = True
estaEnDNI 3 = True
estaEnDNI 8 = True
estaEnDNI _ = False

-- Ejercicio 2
cuentaNoDNI :: [Int] -> Int
cuentaNoDNI [] = 0
cuentaNoDNI (x:xs)
    | not (estaEnDNI x) = 1 + cuentaNoDNI xs
    | otherwise = cuentaNoDNI xs

-- Ejercicio 3
-- Función sumatoria'
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + (sumatoria' xs f)

-- Funcion auxiliar
digitoAContar :: Int -> Int
digitoAContar x
    | not (estaEnDNI x) = 1
    | otherwise = 0

cuentaNoDNI' :: [Int] -> Int
cuentaNoDNI' xs = sumatoria' xs (digitoAContar)

-- Ejercicio 4
aplicaSegun :: [Int] -> Int -> (Int -> a) -> (Int -> a) -> [a]
aplicaSegun [] _ _ _ = []
aplicaSegun (x:xs) n f g
    | x >= n = f x : (aplicaSegun xs n f g)
    | x < n = g x : (aplicaSegun xs n f g)
