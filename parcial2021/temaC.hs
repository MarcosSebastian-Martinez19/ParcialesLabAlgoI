-- Ejercicio 1

data Deporte = Futbol | Basket | Tenis | Valorant | Dota2 deriving Show
type MinJugadores = Int

minimaCantidad :: Deporte -> MinJugadores
minimaCantidad Futbol = 2
minimaCantidad Basket = 3
minimaCantidad Tenis = 2
minimaCantidad Valorant = 5
minimaCantidad Dota2 = 7

-- Ejercicio 2
type NombrePersona = String
data PracticoDeporte = AgregaDeporte Deporte NombrePersona PracticoDeporte | Ninguna

-- Función auxiliar

mismoDeporte :: Deporte -> Deporte -> Bool
mismoDeporte Futbol Futbol = True
mismoDeporte Basket Basket = True
mismoDeporte Tenis Tenis = True
mismoDeporte Valorant Valorant = True
mismoDeporte Dota2 Dota2 = True
mismoDeporte _ _ = False

deporte :: PracticoDeporte -> Deporte -> NombrePersona -> Bool
deporte Ninguna d np = False
deporte(AgregaDeporte depor nom_p ld) d np
    | mismoDeporte depor d && nom_p == np = True || deporte ld d np
    | otherwise = False || deporte ld d np

-- Ejercicio 3
type EquipoFavorito = String
-- Definición de ListaAsoc
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)
    deriving Show

agregaEquipoFavorito :: ListaAsoc Deporte EquipoFavorito -> Deporte -> EquipoFavorito -> ListaAsoc Deporte EquipoFavorito
agregaEquipoFavorito Vacia d ef = Nodo d ef Vacia
agregaEquipoFavorito (Nodo depor equi_fav lef) d ef = Nodo depor equi_fav (agregaEquipoFavorito lef d ef)