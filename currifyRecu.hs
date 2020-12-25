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
    efecto :: [Efecto]
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
genero = "rock melancolico",
duracion = 279  
}

rocketRaccon :: Cancion
rocketRaccon = Cancion{
titulo = "Rocket raccon",
genero = "rap",
duracion = 8  
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
genero = "pop",
duracion = 34 
}

losEscarabajos :: Artista
losEscarabajos = Artista {
nombre = "los escarabajos",
canciones = [rocketRaccon, mientrasMiBateriaFesteja, tomateDeMadera],
efecto = [acortar]
} 

adela :: Artista
adela = Artista {
nombre = "adela",
canciones = [cafeParaDos, fuiHastaAhi],
efecto = [remixar]
} 

elTigreJoaco :: Artista
elTigreJoaco = Artista {
nombre = "el tigre joaco",
canciones = [],
efecto = [acustizar 6]
} 

nico :: Artista
nico = Artista {
  nombre = "Nico",
  canciones = [cafeParaDos,mientrasMiBateriaFesteja,tomateDeMadera,rocketRaccon],
  efecto = [acortar]
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

mapEfecto :: ([Efecto] -> [Efecto]) -> Artista -> Artista
mapEfecto funcionQueModifica unArtista = unArtista {efecto = funcionQueModifica . efecto $ unArtista}

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
  | not $ esDeXGenero "acustico" unaCancion = (mapDuracion (cambiarDuracion duracionCancion). mapGenero (cambiarGenero "acustico")) unaCancion
  | otherwise = unaCancion

esDeXGenero :: Genero -> Cancion -> Bool
esDeXGenero unGenero unaCancion = genero unaCancion == unGenero

-------
metaEfecto :: [Efecto] -> Efecto
metaEfecto listaDeEfectos unaCancion = foldl aplicarUnEfecto unaCancion listaDeEfectos

aplicarUnEfecto :: Cancion -> Efecto -> Cancion
aplicarUnEfecto unaCancion unEfecto = unEfecto unaCancion

listaEfectos :: [Efecto]
listaEfectos = [acortar,acustizar 5]

--------------------
--PARTE B

artistasPio :: [Artista]
artistasPio = [losEscarabajos,adela,nico,elTigreJoaco]

vistazo :: Artista -> [Cancion]
vistazo unArtista = take 3 . filter cancionesCortas $ canciones unArtista

cancionesCortas :: Cancion -> Bool
cancionesCortas unaCancion = duracion unaCancion < 150

playlist :: Genero -> [Artista] -> [Cancion]
playlist unGenero listaDeArtistas = filter (esCancionDeXGenero unGenero) $ cancionesDeTodos listaDeArtistas

cancionesDeTodos :: [Artista] -> [Cancion]
cancionesDeTodos = concatMap canciones 

esCancionDeXGenero :: Genero -> Cancion -> Bool
esCancionDeXGenero unGenero  = (==unGenero) . genero 

---------------------
--PARTE C

hacerseDj :: Artista -> Artista
hacerseDj unArtista = mapCanciones (aplicarEfecto $ head $ efecto unArtista) unArtista

aplicarEfecto :: Efecto -> [CancionesDelArtista] -> [CancionesDelArtista]
aplicarEfecto unEfecto listaDeCanciones = map unEfecto listaDeCanciones
--

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo unArtista = all (esDeXGenero $ genero $ head $ canciones unArtista) $ canciones unArtista
--

formarBanda :: Nombre -> [Artista] -> Artista
formarBanda unNombre listaDeArtista = Artista {nombre = unNombre,canciones = cancionesDeTodos listaDeArtista,efecto = efectosDeTodos listaDeArtista}

efectosDeTodos :: [Artista] -> [Efecto]
efectosDeTodos  = concatMap efecto

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = Cancion{titulo = concatMap titulo $ canciones unArtista,
                                          genero = (generoSuperador(mejorGenero $ listaDeGenerosDelArtista unArtista )) ++ " progresivo",
                                          duracion = sum $ map duracion $ canciones unArtista}

-- generoSuperador tengo que derivarlo que solo reciba un artista y devuelva uyn genero

listaDeGenerosDelArtista :: Artista -> [Genero]
listaDeGenerosDelArtista unArtista =  map genero $ canciones unArtista 

mejorGenero :: [Genero] -> Genero
mejorGenero [] = " "
mejorGenero[genero1] = genero1
mejorGenero (genero1:genero2:otrosGeneros)
 | genero1 == "rock" = genero1
 | genero2 == "rock" = genero2
 | genero1 == "reggaeton" = mejorGenero (genero2:otrosGeneros)
 | genero2 == "reggaeton" = mejorGenero (genero1:otrosGeneros)
 | (length genero1) > (length genero2) = mejorGenero(genero1:otrosGeneros)
 | (length genero2) > (length genero1) = mejorGenero(genero2:otrosGeneros)
 | otherwise = "nati"