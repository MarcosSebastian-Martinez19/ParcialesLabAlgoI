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
cuentaDNI :: [Int] -> Int
cuentaDNI [] = 0
cuentaDNI (x:xs)
    | estaEnDNI x = 1 + cuentaDNI xs
    | otherwise = cuentaDNI xs

-- Ejercicio 3

-- Función sumatoria'
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + (sumatoria' xs f)

-- Funcion auxiliar
digitoAContar :: Int -> Int
digitoAContar x
    | estaEnDNI x = 1
    | otherwise = 0

cuentaDNI' :: [Int] -> Int
cuentaDNI' xs = sumatoria' xs (digitoAContar)

-- Ejercicio 4
separar :: [a] -> (a -> Bool) -> ([a], [a])
separar [] _ = ([], [])
separar (x:xs) predicado
    | predicado x = (x : cumplen, noCumplen)
    | otherwise = (cumplen, x : noCumplen)
    where
        (cumplen, noCumplen) = separar xs predicado