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
sumaDNI :: [Int] -> Int
sumaDNI [] = 0
sumaDNI (x:xs)  | estaEnDNI x = x + sumaDNI xs
                | otherwise = sumaDNI xs

-- Prueba
-- ghci> sumaDNI [1,2,3,4,5]
-- 9
-- ghci> sumaDNI [4,6,7,2,3,8]
-- 30
-- ghci> sumaDNI [1,5,9]
-- 0
-- ghci> sumaDNI [1,3,9]
-- 3

-- Ejercicio 3 Programa mediante composición sin recursion, usando sumatoria' definida en el Proyecto I la funcion

-- Función sumatoria'
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + (sumatoria' xs f)

sumaDNI' :: [Int] -> Int
