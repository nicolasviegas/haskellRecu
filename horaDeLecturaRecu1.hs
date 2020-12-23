import Text.Show.Functions()
import Data.List (genericLength)

main :: IO ()
main = return ()

--type alias!
type Libro = (String, Autor, Int)

elVisitante :: Libro
elVisitante = ("El Visitante", "Stephen King", 592)

shingekiNoKyojin1 :: Libro
shingekiNoKyojin1 = ("Shingeki no Kyojin 1", "Hajime Isayama", 40)

shingekiNoKyojin3 :: Libro
shingekiNoKyojin3 = ("Shingeki no Kyojin 3", "Hajime Isayama", 40)

shingekiNoKyojin27 :: Libro
shingekiNoKyojin27 = ("Shingeki no Kyojin 27", "Hajime Isayama", 40)

fundacion :: Libro
fundacion = ("Fundacion", "Isaac Asimov", 230)

sandman5 :: Libro
sandman5 = ("sandman5", "Neil Gaiman", 35)

sandman10 :: Libro
sandman10 = ("sandman10", "Neil Gaiman", 35)

sandman12 :: Libro
sandman12 = ("sandman12", "Neil Gaiman", 35)

eragon :: Libro
eragon = ("eragon", "Christopher Paolini", 544)

eldest :: Libro
eldest = ("eldest", "Christopher Paolini", 704)

brisignr :: Libro
brisignr = ("brisignr", "Christopher Paolini", 700)

legado :: Libro
legado = ("legado", "Christopher Paolini", 811)

type Saga = [Libro]

sagaDeEragon :: Saga
sagaDeEragon = [eragon, eldest, brisignr, legado]

type Biblioteca = [Libro]

biblioteca :: Biblioteca
biblioteca = [elVisitante, shingekiNoKyojin1, shingekiNoKyojin3, shingekiNoKyojin27, fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado]

promedioDeHojas :: Biblioteca -> Float
promedioDeHojas unaBiblioteca = fromIntegral (cantidadDeHojas unaBiblioteca) / genericLength unaBiblioteca

cantidadDeHojas :: Biblioteca -> Int
cantidadDeHojas unaBiblioteca = (sum . map cantidadDePaginas) unaBiblioteca
 
-- esto es un accessor
cantidadDePaginas :: Libro -> Int
cantidadDePaginas (_, _, unasPaginas) = unasPaginas

autor :: Libro -> String
autor (_, unAutor, _) = unAutor

titulo :: Libro -> String
titulo (unTitulo, _, _) = unTitulo

esLecturaObligatoria :: Libro -> Bool
esLecturaObligatoria unLibro = esDe "Stephen King" unLibro || perteneceASagaEragon unLibro || esFundacion unLibro

type Autor = String

esDe :: Autor -> Libro -> Bool
esDe unAutor unLibro = ((== unAutor) . autor) unLibro

perteneceASagaEragon :: Libro -> Bool
perteneceASagaEragon unLibro = elem unLibro sagaDeEragon

esFundacion :: Libro -> Bool
esFundacion ("fundacion", "Isaac Asimov", 230) = True
esFundacion _                                  = False

esFundacion' :: Libro -> Bool
esFundacion' unLibro = unLibro == fundacion

esLecturaObligatoria' :: Libro -> Bool
esLecturaObligatoria' (_, "Stephen King", _)             = True
esLecturaObligatoria' ("fundacion", "Isaac Asimov", 230) = True
esLecturaObligatoria' unLibro                            = perteneceASagaEragon unLibro

esFantasiosa :: Biblioteca -> Bool
esFantasiosa unaBiblioteca = any esLibroFantasioso unaBiblioteca

esFantasiosa' :: Biblioteca -> Bool
esFantasiosa' unaBiblioteca = any (esDe "Christopher Paolini") unaBiblioteca || any (esDe "Neil Gaiman") unaBiblioteca

esLibroFantasioso :: Libro -> Bool
esLibroFantasioso (_, unAutor, _) = elem unAutor ["Cristopher Paolini", "Neil Gaiman"]

esLibroFantasioso' :: Libro -> Bool
esLibroFantasioso' unLibro = esDe "Christopher Paolini" unLibro || esDe "Neil Gaiman" unLibro

nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca unaBiblioteca = (sacarVocales . nombreDeLaBibliotecaConVocales) unaBiblioteca

sacarVocales :: String -> String
sacarVocales unTitulo = filter esConsonante unTitulo

esConsonante :: Char -> Bool
esConsonante unCaracter = (not . elem unCaracter) "aeiouAEIOU"

nombreDeLaBibliotecaConVocales :: Biblioteca -> String
nombreDeLaBibliotecaConVocales unaBiblioteca = concatMap titulo unaBiblioteca

esBibliotecaLigera :: Biblioteca -> Bool
esBibliotecaLigera unaBiblioteca = all esLibroLigero unaBiblioteca

esLibroLigero :: Libro -> Bool
esLibroLigero unLibro = ((<= 40) . cantidadDePaginas) unLibro