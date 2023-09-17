-- Ejercicio 1
-- a

data Palo = Treboles | Corazones | Picas | Diamantes
    deriving (Eq,Ord)

mismo_palo :: Palo -> Palo -> Bool
mismo_palo Treboles Treboles = True
mismo_palo Corazones Corazones = True
mismo_palo Picas Picas = True
mismo_palo Diamantes Diamantes = True
mismo_palo _ _ = False

-- b

data Naipe = Numerada Numero Palo | Rey Palo | Reina Palo | Jota Palo | As Palo
    deriving Ord

type Numero = Int

-- c
valor_naipe :: Naipe -> Int
valor_naipe (Numerada numero _) = numero
valor_naipe (Jota _) = 11
valor_naipe (Reina _) = 12
valor_naipe (Rey _) = 13
valor_naipe (As _) = 14

-- d

instance Eq Naipe where
    (Numerada numero1 palo1) == (Numerada numero2 palo2) = valor_naipe (Numerada numero1 palo1) == valor_naipe (Numerada numero2 palo2)
    (Jota palo1) == (Jota palo2) = valor_naipe (Jota palo1) == valor_naipe (Jota palo2)
    (Reina palo1) == (Reina palo2) = valor_naipe (Reina palo1) == valor_naipe (Reina palo2)
    (Rey palo1) == (Rey palo2) = valor_naipe (Rey palo1) == valor_naipe (Rey palo2)
    (As palo1) == (As palo2) = valor_naipe (As palo1) == valor_naipe (As palo2)

-- Pruebas
--ghci> (Numerada 2 Corazones) <= (Numerada 4 Diamantes)
--True
--ghci> (Numerada 2 Corazones) == (Numerada 4 Diamantes)
--False
--ghci> (Numerada 2 Corazones) == (Numerada 2 Diamantes)
--True

-- Ejercicio 2
-- a

solo_numeradas :: [Naipe] -> Palo -> [Numero]
solo_numeradas [] p = []
solo_numeradas ((Numerada numero palo):ns) p = case mismo_palo palo p of
    True -> numero : (solo_numeradas ns p)
    False -> solo_numeradas ns p
solo_numeradas (_:ns) p = solo_numeradas ns p

-- b
listaNaipe :: [Naipe]
listaNaipe = [(Numerada 2 Corazones), (Rey Corazones), (Numerada 9 Corazones),(Numerada 3 Corazones),(Numerada 5 Treboles)]

-- c
-- ghci> solo_numeradas listaNaipe Corazones
-- [2,9,3]

-- Ejercicio 3

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)
    deriving (Eq, Ord, Show)

la_menores :: (Eq b, Ord b) => ListaAsoc a b -> b -> ListaAsoc a b
la_menores Vacia x = Vacia
la_menores (Nodo y z la) x
    | z < x = Nodo y z (la_menores la x)
    | z >= x = la_menores la x

-- Pruebas
-- ghci> la_menores (Nodo 'a' 1 (Nodo 'b' 2 (Nodo 'c' 3 (Nodo 'd' 4 Vacia)))) 3
-- Nodo 'a' 1 (Nodo 'b' 2 Vacia)