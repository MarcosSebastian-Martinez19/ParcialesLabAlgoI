-- Recuperatorio

-- Ejercicio 1

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + (sumatoria' xs f)

suma_multiplos :: [Int] -> Int -> Int
suma_multiplos [] m = 0
suma_multiplos (x:xs) m
    | esMultiplo m x = sumatoria' [x] id
    | otherwise = suma_multiplos xs m

-- Pruebas
-- ghci> suma_multiplos [2,4,5,6] 2
-- 12
-- ghci> suma_multiplos [2,4,5,6] 4
-- 4
-- ghci> suma_multiplos [2,4,5,6] 5
-- 5
-- ghci> suma_multiplos [2,4,5,6] 6
-- 6
-- ghci> suma_multiplos [2,4,5,6] 3
-- 6

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

suma_multiplos' :: [Int] -> Int -> Int
suma_multiplos' [] m = 0
suma_multiplos' xs m = sumatoria (filter (esMultiplo m) xs)

-- Funcion auxiliar
esMultiplo :: Int -> Int -> Bool
esMultiplo x y
    | y `mod` x == 0 = True
    | y `mod` x /= 0 = False

-- Pruebas
-- ghci> suma_multiplos' [2,4,5,6] 2
-- 12
-- ghci> suma_multiplos' [2,4,5,6] 4
-- 4
-- ghci> suma_multiplos' [2,4,5,6] 5
-- 5
-- ghci> suma_multiplos' [2,4,5,6] 7
-- 0
-- ghci> suma_multiplos' [2,4,5,6] 6
-- 6
-- ghci> suma_multiplos' [2,4,5,6] 3
-- 6

-- Ejercicio 2

data Carrera = Matematica | Astronomia | Fisica | Computacion
type Nombre = String
type Legajo = Int

data Estudiante = Est Legajo Nombre Carrera

buscar :: [Estudiante] -> Carrera -> [Nombre]
buscar [] _ = []
buscar ((Est _ nombre Matematica):xs) Matematica = nombre : (buscar xs Matematica)
buscar ((Est _ nombre Astronomia):xs) Astronomia = nombre : (buscar xs Astronomia)
buscar ((Est _ nombre Fisica):xs) Fisica = nombre : (buscar xs Fisica)
buscar ((Est _ nombre Computacion):xs) Computacion = nombre : (buscar xs Computacion)
buscar (_:xs) carrera = buscar xs carrera

-- Pruebas
-- ghci> buscar [(Est 1 "Marcos" Matematica), (Est 2 "Seba" Astronomia), (Est 3 "Alex" Fisica),(Est 3 "Fer" Matematica)] Matematica
-- ["Marcos","Fer"]

-- Ejercicio 3

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)
    deriving Show

-- a
la_existe :: Eq a => ListaAsoc a b -> a -> Bool
la_existe Vacia k = False
la_existe (Nodo x y la) k = x == k || la_existe la k

-- b
-- ghci> la_existe (Nodo 'a' 2 (Nodo 'b' 3 (Nodo 'c' 4 (Nodo 'd' 5 Vacia)))) 'a'
-- True
-- ghci> la_existe (Nodo 'a' 2 (Nodo 'b' 3 (Nodo 'c' 4 (Nodo 'd' 5 Vacia)))) 'b'
-- True
-- ghci> la_existe (Nodo 'a' 2 (Nodo 'b' 3 (Nodo 'c' 4 (Nodo 'd' 5 Vacia)))) 'c'
-- True
-- ghci> la_existe (Nodo 'a' 2 (Nodo 'b' 3 (Nodo 'c' 4 (Nodo 'd' 5 Vacia)))) 'd'
-- True
-- ghci> la_existe (Nodo 'a' 2 (Nodo 'b' 3 (Nodo 'c' 4 (Nodo 'd' 5 Vacia)))) '3'
-- False
-- ghci> la_existe (Nodo 'a' 2 (Nodo 'b' 3 (Nodo 'c' 4 (Nodo 'd' 5 Vacia)))) 'e'
-- False