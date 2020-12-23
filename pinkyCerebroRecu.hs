import Text.Show.Functions()

type Nombre = String
type Edad = Float 
type Peso = Float
type Enfermedad = String


data Raton = Raton {
nombre :: Nombre,
edad :: Edad,
peso:: Peso,
enfermedad :: [Enfermedad]
} deriving Show

cerebro :: Raton
cerebro = Raton {
nombre = "Cerebro",
edad = 9,
peso = 0.2,
enfermedad = ["brucelosis","sarampion","tuberculosis"]
}

bicenterrata :: Raton
bicenterrata = Raton {
nombre = "Bicenterrata",
edad = 256,
peso = 0.2,
enfermedad = []
}

huesudo :: Raton
huesudo = Raton {
nombre = "Huesudo",
edad = 4,
peso = 10,
enfermedad = ["obesidad","sinusitis"]
}

--------Functores---------
mapNombre :: (String -> String) -> Raton -> Raton
mapNombre modificarElNombre unRaton = unRaton {nombre = modificarElNombre . nombre $ unRaton}

mapEdad :: (Float -> Float) -> Raton -> Raton
mapEdad modificarEdad unRaton = unRaton {edad = modificarEdad . edad $ unRaton}

mapPeso :: (Float -> Float) -> Raton -> Raton
mapPeso modificarElPeso unRaton = unRaton {peso = modificarElPeso . peso $ unRaton}

mapEnfemedad :: ([Enfermedad] -> [Enfermedad]) -> Raton -> Raton
mapEnfemedad modificarEnfemedad unRaton = unRaton {enfermedad = modificarEnfemedad . enfermedad $ unRaton}

---------------------------
type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena  = mapEdad (sqrt) 
--
hierbaVerde :: String -> Hierba
hierbaVerde terminacion unRaton = mapEnfemedad (eliminarConTerminacion $ terminacion) unRaton

eliminarConTerminacion :: String -> [String] -> [String]
eliminarConTerminacion terminacion listaDePalabras = filter (not . mismaTerminacion terminacion ) listaDePalabras

mismaTerminacion :: String -> String -> Bool
mismaTerminacion terminacion unaPalabra = terminacion == losUltimos (length terminacion) unaPalabra

losUltimos :: Int -> [a] -> [a]
losUltimos largoDeLaTerminacion palabra = drop (length palabra - largoDeLaTerminacion) palabra
--
alcachofa :: Hierba
alcachofa unRaton 
  | peso unRaton > 2 = mapPeso (restarXPorciento 10) unRaton
  | otherwise = mapPeso (restarXPorciento 5) unRaton

restarXPorciento :: Float -> Float -> Float 
restarXPorciento porcentaje pesoRaton = (porcentaje * pesoRaton) / 100
--
hierbaZort :: Hierba
hierbaZort unRaton = unRaton {nombre = "Pinky", edad = 0, peso = peso unRaton, enfermedad = []}
--
hierbaDelDiablo :: Hierba
hierbaDelDiablo = mapEnfemedad (eliminarConMenosDeXLetras 10) . mapPeso (perderXpeso 0.1) 

perderXpeso :: Float -> Float -> Float
perderXpeso pesoAPerder pesoRaton 
  | pesoRaton >= pesoAPerder = pesoRaton - pesoAPerder
  | otherwise = 0

eliminarConMenosDeXLetras :: Int -> [Enfermedad] -> [Enfermedad]
eliminarConMenosDeXLetras cantdidadLimite = filter ((>cantdidadLimite) . length) 

----------------------------------------------------

type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge = alcachofa . hierbaBuena . hierbaBuena . hierbaBuena

reduceFatFast :: Int -> Medicamento 
reduceFatFast potencia = (alcachofaPotenciada potencia) . hierbaVerde "obesidad"

alcachofaPotenciada :: Int -> Raton -> Raton
alcachofaPotenciada potencia raton = last $ take potencia (iterate alcachofa raton)

pdepCilina :: Medicamento
pdepCilina unRaton = foldr ($) unRaton (map(hierbaVerde) sufijosInfecciosas)

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina2 :: Medicamento
pdepCilina2 unRaton = foldl curarConSufijo unRaton sufijosInfecciosas

curarConSufijo :: Raton -> String -> Raton
curarConSufijo unRaton unSufijo = hierbaVerde unSufijo unRaton


---------------------------
type Comunidad = [Raton]
comunidad :: Comunidad
comunidad = [cerebro,bicenterrata,huesudo]

comunidad2 :: Comunidad
comunidad2 = [cerebro,bicenterrata]
------
cantidadIdeal :: (Int -> Bool) -> Int
cantidadIdeal condicion = head (filter condicion [1..])
-----

aplicarMedicamento :: Medicamento -> Comunidad -> Comunidad
aplicarMedicamento unMedicamento comunidadRatones = map unMedicamento comunidadRatones

noTieneSobrePeso :: Raton -> Bool
noTieneSobrePeso unRaton = peso unRaton <= 1

tieneMenosDe3Enfemedades :: Raton -> Bool
tieneMenosDe3Enfemedades unRaton = (<3). length $ enfermedad unRaton

comunidadSinXCondicion :: (Raton -> Bool) -> Comunidad -> Bool
comunidadSinXCondicion condicion comunidadRatones = length comunidadRatones == (length . filter condicion) comunidadRatones

lograEstabilizar :: Medicamento -> Comunidad -> Bool
lograEstabilizar unMedicamento unaComunidad = (comunidadSinXCondicion noTieneSobrePeso $ aplicarMedicamento unMedicamento unaComunidad) == (comunidadSinXCondicion tieneMenosDe3Enfemedades $ aplicarMedicamento unMedicamento unaComunidad)

--------------

lograEstabilizarReduceFatFast :: Comunidad -> Int
lograEstabilizarReduceFatFast ratones = cantidadIdeal (\potencia -> lograEstabilizar (reduceFatFast potencia) ratones)
