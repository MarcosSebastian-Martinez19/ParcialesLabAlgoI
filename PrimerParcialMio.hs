-- Ejercicio 2
-- a)
data TipoLadrillo = Ceramico | Hormigon | Tradicional deriving Show
data UsoDeLadrillo = Pared | Techo deriving Show
type Precio = Int
type Largo = Float
data MaterialViga = Hierro | Madera deriving Show
data MarcaCemento = Minetti | LomaNegra deriving Show

data MaterialesDeConstruccion = Ladrillo TipoLadrillo UsoDeLadrillo Precio | Vigueta Largo MaterialViga Precio | Cemento MarcaCemento Precio deriving Show

-- b)

ladrilloDeMenorPrecio :: [MaterialesDeConstruccion] -> Int -> [MaterialesDeConstruccion]
ladrilloDeMenorPrecio [] _ = []
ladrilloDeMenorPrecio (Ladrillo t u p : lm) n
    | p <= n = (Ladrillo t u p) : ladrilloDeMenorPrecio lm n
    | otherwise = ladrilloDeMenorPrecio lm n
ladrilloDeMenorPrecio ( _ : lm) n = ladrilloDeMenorPrecio lm n

-- c)
mismoTipo :: TipoLadrillo -> TipoLadrillo -> Bool
mismoTipo Ceramico Ceramico = True
mismoTipo Hormigon Hormigon = True
mismoTipo Tradicional Tradicional = True
mismoTipo _ _ = False

mismoMaterial :: MaterialViga -> MaterialViga -> Bool
mismoMaterial Hierro Hierro = True
mismoMaterial Madera Madera = True
mismoMaterial _ _ = False

mismaMarca :: MarcaCemento -> MarcaCemento -> Bool
mismaMarca Minetti Minetti = True
mismaMarca LomaNegra LomaNegra = True
mismaMarca _ _ = False

instance Eq MaterialesDeConstruccion where
    (Ladrillo o _ p) == (Ladrillo o1 _ p1) = mismoTipo o o1 && p == p1
    (Vigueta l m _) == (Vigueta l1 m1 _) = mismoMaterial m m1 && l == l1
    (Cemento m _) == (Cemento m1 _) = mismaMarca m m1
    _ == _ = False

-- Ejercicio 3
-- a)

data Estado = Seco | EnFlor | Verde | ConFrutos deriving Show
data ArbolesNativos = NoHayMasArboles | EvolucionDelArbol String Estado Int Int Int ArbolesNativos deriving Show

-- funciones auxiliares

estadoConFrutos :: Estado -> Bool
estadoConFrutos ConFrutos = True
estadoConFrutos _ = False

estadoEnFlor :: Estado -> Bool
estadoEnFlor EnFlor = True
estadoEnFlor _ = False

estadoVerde :: Estado -> Bool
estadoVerde Verde = True
estadoVerde _ = False

-- b)

paraVender :: ArbolesNativos -> String -> Bool
paraVender NoHayMasArboles _ = False
paraVender (EvolucionDelArbol n e a d ap resto) nombre
    | n == nombre = estadoConFrutos e || (estadoEnFlor e && (d > 7 || a > 7) && ap >= 8) || (estadoVerde e && d >= 9 && a >= 9 && ap >= 9) || paraVender resto nombre
    | otherwise = paraVender resto nombre

-- c)

devolverAltura :: ArbolesNativos -> String -> Maybe Int
devolverAltura NoHayMasArboles _ = Nothing
devolverAltura (EvolucionDelArbol n _ a _ _ resto) nombre
    | n == nombre = Just a
    | otherwise = devolverAltura resto nombre
