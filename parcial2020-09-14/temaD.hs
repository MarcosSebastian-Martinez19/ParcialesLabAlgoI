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
cuentaEnApellido (x:xs)
    | letraEnApellido x = 1 + cuentaEnApellido xs
    | otherwise = cuentaEnApellido xs

-- Ejercicio 3
-- Función sumatoria'
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + (sumatoria' xs f)

-- Funcion auxiliar
letraAContar :: Char -> Int
letraAContar x
    | letraEnApellido x = 1
    | otherwise = 0

cuentaEnApellido' :: [Char] -> Int
cuentaEnApellido' xs = sumatoria' xs (letraAContar)

-- Ejercicio 4
alterna :: [a] -> (a -> b) -> (a -> b) -> [b]
alterna [] _ _ = []
alterna (x:y:xs) f g = f x : g y : (alterna xs f g)