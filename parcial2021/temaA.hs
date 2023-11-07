-- Ejercicio 1
data EmpresaTelefono = Claro | Personal | Movistar | Tuenti deriving Show
type Frase = String

fraseEmpresa :: EmpresaTelefono -> Frase
fraseEmpresa Claro = "Claro, La red mas poderosa."
fraseEmpresa Personal = "Personal, es como vos."
fraseEmpresa Movistar = "Movistar, Compartida la vida es mas..."
fraseEmpresa Tuenti = "Tuenti es la mas economica."

-- Ejercicio 2
type NombrePersona = String

data MisEmpresas = AgregaEmpresa EmpresaTelefono NombrePersona MisEmpresas | Ninguna

mismaEmpresa :: EmpresaTelefono -> EmpresaTelefono -> Bool
mismaEmpresa Claro Claro = True
mismaEmpresa Personal Personal = True
mismaEmpresa Movistar Movistar = True
mismaEmpresa Tuenti Tuenti = True
mismaEmpresa _ _ = False

tengoEmpresa :: MisEmpresas -> EmpresaTelefono -> NombrePersona -> Bool
tengoEmpresa Ninguna _ _ = False
tengoEmpresa(AgregaEmpresa empresa nombre misempresas) empresa_p nombre_p
    | mismaEmpresa empresa empresa_p && nombre == nombre_p = True || tengoEmpresa misempresas empresa_p nombre_p 
    | otherwise = False || tengoEmpresa misempresas empresa_p nombre_p

-- Ejercicio 3
type NroTel = Int
-- Definición de ListaAsoc
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)
    deriving Show

agregaLA :: ListaAsoc EmpresaTelefono NroTel -> EmpresaTelefono -> NroTel -> ListaAsoc EmpresaTelefono NroTel
agregaLA Vacia e n = Nodo e n Vacia
agregaLA (Nodo empresa nro la ) e n = Nodo empresa nro (agregaLA la e n)
