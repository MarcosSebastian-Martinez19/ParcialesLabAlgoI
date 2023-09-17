-- Tema A
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

-- Funcion auxiliar
digitoASumar :: Int -> Int
digitoASumar x
    | estaEnDNI x = x
    | otherwise = 0

sumaDNI' :: [Int] -> Int
sumaDNI' xs = sumatoria' xs digitoASumar

-- Prueba
--ghci> sumaDNI' [4,6,7,2,3,8]
--30
--ghci> sumaDNI' [1,3,9]
--3

-- Ejercicio 4

reducir :: [a] -> (a -> a -> a) -> a
reducir [x] op = x
reducir (x : y :xs) op = reducir (x `op` y :xs) op

-- Prueba
--ghci> reducir [3,4,5,6] (+)
--18
--ghci> reducir [3,4,5,6] (*)
--360

-- Tema B
-- Ejercicio 2
cuentaDNI :: [Int] -> Int
cuentaDNI [] = 0
cuentaDNI (x:xs) | estaEnDNI x = 1 + cuentaDNI xs
               | otherwise = cuentaDNI xs
-- Prueba
-- ghci> cuentaDNI [1,2,3,4,5,6,7,8,9]
-- 6
-- ghci> cuentaDNI [1,2,3,4,5,6,7]
-- 5
-- ghci> cuentaDNI []
-- 0

-- Ejercicio 3
digitoAContar :: Int -> Int
digitoAContar x
    | estaEnDNI x = 1
    | otherwise = 0
cuentaDNI' :: [Int] -> Int
cuentaDNI' xs = sumatoria' xs digitoAContar 

-- Prueba
--ghci> cuentaDNI' [1,2,3,4,5,6,7,8,9]
--6
--ghci> cuentaDNI' []
--0

-- Ejercicio 4

tieneA :: Char -> Bool
tieneA a = True

separar :: [a] -> (a -> Bool) -> ([a], [a])
separar [] _ = ([], [])
separar (x:xs) predicado
    | predicado x = (x: cumplen, noCumplen)
    | otherwise = (cumplen, x : noCumplen)
    where
        (cumplen, noCumplen) = separar xs predicado

-- Prueba
--ghci> separar [3,4,5,6] odd
--([3,5],[4,6])

-- Tema C
-- Ejercicio 2
cuentaNoDNI :: [Int] -> Int
cuentaNoDNI [] = 0
cuentaNoDNI (x:xs) | not (estaEnDNI x) = 1 + cuentaNoDNI xs
                   | otherwise = cuentaNoDNI xs
-- Prueba
--ghci> cuentaNoDNI [1,2,3,4,5,6,7]
--2
--ghci> cuentaNoDNI []
--0
--ghci> cuentaNoDNI [1,2,3,4,5,6,7,8,9]
--3

-- Ejercicio 3
digitoANoContar :: Int -> Int
digitoANoContar x
    | estaEnDNI x = 0
    | otherwise = 1

cuentaNoDNI' :: [Int] -> Int
cuentaNoDNI' xs = sumatoria' xs digitoANoContar

-- Prueba
--ghci> cuentaNoDNI' [1,2,3,4,5,6,7,8,9]
--3
--ghci> cuentaNoDNI' []
--0

-- Ejercicio 4

aplicaSegun :: [Int] -> Int -> (Int -> a) -> (Int -> a) -> [a]
aplicaSegun [] _ _ _ = [] 
aplicaSegun (x:xs) n f g
    | x >= n = f x : aplicaSegun xs n f g
    | x < n = g x : aplicaSegun xs n f g

-- Pruebas
--ghci> aplicaSegun [1,3,4,5,6,2] 4 (+2) (*2)
--[2,6,6,7,8,4]
--ghci> aplicaSegun [3,1,5,7] 4 even odd
--[True,True,False,False]

-- Tema D
-- Ejercicio 1
letraEnApellido :: Char -> Bool
letraEnApellido 'm' = True
letraEnApellido 'a' = True
letraEnApellido 'r' = True
letraEnApellido 't' = True
letraEnApellido 'i' = True
letraEnApellido 'n' = True
letraEnApellido 'e' = True
letraEnApellido 'z' = True
letraEnApellido _ = False

-- Ejercicio 2
cuentaEnApellido :: [Char] -> Int
cuentaEnApellido [] = 0
cuentaEnApellido (x:xs) | letraEnApellido x = 1 + cuentaEnApellido xs
                        | otherwise = cuentaEnApellido xs

-- Prueba
-- Apellido usado: martinez
-- ghci> cuentaEnApellido "Suerte en el parcialito"
-- 13
-- ghci> cuentaEnApellido "trrG"
-- 3
-- ghci> cuentaEnApellido "martinez2"
-- 8
-- ghci> cuentaEnApellido "martinezz"
-- 9
-- ghci> cuentaEnApellido "martinezG"
-- 8
-- ghci> cuentaEnApellido "martinezZ"
-- 8

-- Ejercicio 3
letraAContar :: Char -> Int
letraAContar x
    | letraEnApellido x = 1
    | otherwise = 0

cuentaEnApellido' :: [Char] -> Int
cuentaEnApellido' xs = sumatoria' xs letraAContar

-- Prueba
-- ghci> cuentaEnApellido' "Suerte en el parcialito"
-- 13
-- ghci> cuentaEnApellido' "trrG"
-- 3
-- ghci> cuentaEnApellido' "martinez2"
-- 8

-- Ejercicio 4

alterna :: [a] -> (a -> b) -> (a -> b) -> [b]
alterna [] _ _ = []
alterna [x] f _ = [f x]
alterna (x:y:xs) f g = f x : g y : alterna xs f g

-- Prueba
--ghci> alterna [1,2,3,4] (+1) (*2)
--[2,4,4,8]
--ghci> alterna [1,1,1,1] odd even
--[True,False,True,False]