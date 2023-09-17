-- Tema a

-- Piedra Papel Tijera
-- a) Definir el tipo Forma que consta de los constructores sin parametricos Piedra, Papel y Tijera. El tipo forma no debe estar en la clase Eq. Luego programar la función.
data Forma = Piedra | Papel | Tijera
    deriving Show

le_gana :: Forma -> Forma -> Bool
le_gana Piedra Tijera = True
le_gana Piedra Piedra = False
le_gana Piedra Papel = False
le_gana Papel Piedra = True
le_gana Papel Papel = False
le_gana Papel Tijera = False
le_gana Tijera Papel = True
le_gana Tijera Tijera = False
le_gana Tijera Piedra = False

-- b)
type Nombre = String
data Jugador = Mano Nombre Forma

ganador :: Jugador -> Jugador -> Maybe Nombre
ganador (Mano j1 forma1) (Mano j2 forma2) | le_gana forma1 forma2 = Just (j1)
                                          | otherwise = Nothing

-- Ejercicio 2

quien_jugo :: Forma -> [Jugador] -> [Nombre]
quien_jugo forma [] = []
quien_jugo forma ((Mano nombre forma2):js) | compararFormas forma forma2 = nombre : (quien_jugo forma js)
                                           | otherwise = quien_jugo forma js

compararFormas :: Forma -> Forma -> Bool
compararFormas Piedra Piedra = True
compararFormas Papel Papel = True
compararFormas Tijera Tijera = True
compararFormas _ _ = False

-- Ejemplo
--ghci> quien_jugo Piedra [(Mano "Marcos" Piedra), (Mano "Seba" Piedra), (Mano "Martin" Papel), (Mano "Martinez" Piedra)]
--["Marcos","Seba","Martinez"]

-- Ejercicio 3
data NotaMusical = Do | Re | Mi | Fa | Sol | La | Si
    deriving Show

data Figura = Negra | Corchea
    deriving Show

data Melodia = Entonar NotaMusical Figura Melodia | Vacia
    deriving Show

contar_tiempos :: Melodia -> Int
contar_tiempos Vacia = 0
contar_tiempos (Entonar _ figura melodia) = case figura of 
        Negra -> 2 + contar_tiempos melodia
        Corchea -> 1 + contar_tiempos melodia

-- Pruebas
--ghci> let pink = Entonar Re Negra (Entonar Mi Corchea (Entonar Fa Negra (Entonar Mi Negra Vacia)))
--ghci> contar_tiempos pink
--7

-- Tema b
-- Ejercicio 1
-- a

data Palo = Espada | Basto | Oro | Copa

mismo_palo :: Palo -> Palo -> Bool
mismo_palo Espada Espada = True
mismo_palo Basto Basto = True
mismo_palo Oro Oro = True
mismo_palo Copa Copa = True
mismo_palo _ _ = False

-- b
data Figura' = Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Sota | Caballo | Rey

valor_figura :: Figura' -> Int
valor_figura Uno = 1
valor_figura Dos = 2
valor_figura Tres = 3
valor_figura Cuatro = 4
valor_figura Cinco = 5
valor_figura Seis = 6
valor_figura Siete = 7
valor_figura Sota = 8
valor_figura Caballo = 9
valor_figura Rey = 10

data Carta = Naipe Figura' Palo

suma_cartas :: Carta -> Carta -> Maybe Int
suma_cartas (Naipe figura1 palo1) (Naipe figura2 palo2)
    | mismo_palo palo1 palo2 = Just ((valor_figura figura1) + (valor_figura figura2))
    | otherwise = Nothing
