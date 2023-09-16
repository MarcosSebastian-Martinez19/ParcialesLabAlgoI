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