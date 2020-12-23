import Text.Show.Functions()

type Titulo = String
type Genero = String
type Duracion = Int

type Nombre = String
type CancionesDelArtista = Cancion
type Efecto = Cancion -> Cancion

data Cancion = Cancion {
    titulo :: Titulo,
    genero :: Genero,
    duracion :: Duracion
}deriving Show

data Artista = Artista {
    nombre :: Nombre,
    canciones :: [CancionesDelArtista],
    efectos :: [Efecto]
} deriving Show


cafeParaDos :: Cancion
cafeParaDos = Cancion{
titulo = "Cafe para dos",
genero = "rock melancolico",
duracion = 146  
}

fuiHastaAhi :: Cancion
fuiHastaAhi = Cancion{
titulo = "Fui hasta ahi",
genero = "rocks",
duracion = 279  
}

rocketRaccon :: Cancion
rocketRaccon = Cancion{
titulo = "Rocket raccon",
genero = "rap",
duracion = 345  
}

mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = Cancion{
titulo = "Mientras mi bateria festeja",
genero = "pop",
duracion = 52  
}

tomateDeMadera :: Cancion
tomateDeMadera = Cancion{
titulo = "Tomate de madera",
genero = "clasico",
duracion = 482 
}

losEscarabajos :: Artista
losEscarabajos = Artista {
nombre = "los escarabajos",
canciones = [rocketRaccon, mientrasMiBateriaFesteja, tomateDeMadera],
efectos = [acortar]
} 

adela :: Artista
adela = Artista {
nombre = "adela",
canciones = [cafeParaDos, fuiHastaAhi],
efectos = [remixar]
} 

elTigreJoaco :: Artista
elTigreJoaco = Artista {
nombre = "el tigre joaco",
canciones = [],
efectos = [acustizar 6]
} 

-------------Functores----------------
--Cancion
mapTitulo :: (Titulo -> Titulo) -> Cancion -> Cancion
mapTitulo funcionQueModifica unaCancion = unaCancion {titulo = funcionQueModifica . titulo $ unaCancion}

mapGenero :: (Genero -> Genero) -> Cancion -> Cancion 
mapGenero funcionQueModifica unaCancion = unaCancion {genero = funcionQueModifica . genero $ unaCancion}

mapDuracion :: (Duracion -> Duracion) -> Cancion -> Cancion 
mapDuracion funcionQueModifica unaCancion = unaCancion {duracion = funcionQueModifica . duracion $ unaCancion}

--Artista
mapNombre :: (Nombre -> Nombre) -> Artista -> Artista
mapNombre funcionQueModifica unArtista = unArtista {nombre = funcionQueModifica . nombre $ unArtista}

mapCanciones :: ([CancionesDelArtista] -> [CancionesDelArtista])  -> Artista -> Artista
mapCanciones funcionQueModifica unArtista = unArtista {canciones = funcionQueModifica . canciones $ unArtista}

mapEfectos :: ([Efecto] -> [Efecto]) -> Artista -> Artista
mapEfectos funcionQueModifica unArtista = unArtista {efectos = funcionQueModifica . efectos $ unArtista}

----------------
--PARTE A 

acortar :: Efecto
acortar unaCancion = mapDuracion (max 0 . ((flip (-))) 60) unaCancion

remixar :: Efecto
remixar unaCancion = (mapTitulo (++" remix") . (mapDuracion . (*)) 2 . mapGenero (cambiarGenero "remix")) unaCancion

--remixar2 :: Efecto
--remixar2 unaCancion = (mapTitulo (++" remix") . (mapDuracion . (*)) 2 . cambiarGenero "Remix") unaCancion

--cambiarGenero :: Genero -> Cancion -> Cancion
--cambiarGenero generoNuevo unaCancion = unaCancion{genero = generoNuevo}

------
cambiarGenero :: String -> String -> String
cambiarGenero generoNuevo _ = generoNuevo

cambiarDuracion :: Int -> Int -> Int
cambiarDuracion duracionNueva _ = duracionNueva
-------
acustizar :: Int -> Efecto
acustizar duracionCancion unaCancion 
  | noEsAcustica unaCancion = (mapDuracion (cambiarDuracion duracionCancion). mapGenero (cambiarGenero "acustico")) unaCancion
  | otherwise = unaCancion

noEsAcustica :: Cancion -> Bool
noEsAcustica unaCancion = genero unaCancion /= "acustico"
-------
metaEfecto :: [Efecto] -> Efecto
metaEfecto listaDeEfectos unaCancion = foldl aplicarUnEfecto unaCancion listaDeEfectos

aplicarUnEfecto :: Cancion -> Efecto -> Cancion
aplicarUnEfecto unaCancion unEfecto = unEfecto unaCancion

listaEfectos :: [Efecto]
listaEfectos = [acortar,acustizar 5]

--------------------
--PARTE B