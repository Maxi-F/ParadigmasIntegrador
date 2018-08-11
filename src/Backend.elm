module Backend exposing(..)
import Models exposing(Movie, Preferences)
import List exposing(filter, map, sortBy, member, foldr, reverse, any, all)
import String exposing (contains, split, toLower)

completaAca = identity

abecedarioYNumeros = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

ordenarDescendientemente : (Movie -> comparable) -> List Movie -> List Movie
ordenarDescendientemente criterio = reverse << (sortBy criterio)

pasarPalabrasAMinusculas : List String -> List String
pasarPalabrasAMinusculas = map toLower

dividirPalabrasPorEspacios : String -> List String
dividirPalabrasPorEspacios = split " "

esLetraONumero : Char -> Bool
esLetraONumero caracter = member caracter abecedarioYNumeros

filtrarCaracteresEspeciales : List String -> List String
filtrarCaracteresEspeciales = map (String.filter esLetraONumero)

hacerComparable : String -> List String
hacerComparable = filtrarCaracteresEspeciales << pasarPalabrasAMinusculas << dividirPalabrasPorEspacios

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras peliculas = if palabras /= "" then filter (peliculaTienePalabrasClave palabras) peliculas
                                                      else peliculas

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--

esCoincidenciaClave : String -> String -> Bool
esCoincidenciaClave palabraDelTitulo palabra = if palabra /= "" then contains palabra palabraDelTitulo
                                               else False
--algunaEsPalabraClave : List String -> String -> Bool
--algunaEsPalabraClave palabras palabraDelTitulo = any (esPalabraclave palabraDelTitulo) palabras

sonPalabrasClave : List String -> String -> Bool
sonPalabrasClave palabras palabraDelTitulo = any (esCoincidenciaClave palabraDelTitulo) palabras

peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula = any (sonPalabrasClave (hacerComparable palabras)) (hacerComparable pelicula.title)

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = filter (esPeliculaDeGenero genero)

esPeliculaDeGenero : String -> Movie -> Bool
esPeliculaDeGenero genero pelicula = member genero pelicula.genre

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores peliculas = if mostrarSoloMenores then soloParaMenores peliculas
                                                                else peliculas

soloParaMenores : List Movie -> List Movie
soloParaMenores = filter << forKids

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = ordenarDescendientemente .rating

-- **************
-- Requerimiento: dar like a una película
-- **************

sumarLike : Int -> Movie -> Movie
sumarLike id pelicula = if pelicula.id == id then { pelicula | likes = pelicula.likes + 1 }
                        else pelicula

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = map (sumarLike id)

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

volverACalcular : Movie -> Movie
volverACalcular pelicula = {pelicula | matchPercentage = 0}

comparacionConPreferencia : Int -> List String -> String -> Movie -> Movie
comparacionConPreferencia porcentaje listaAComparar preferencia pelicula = if (member (toLower preferencia) (pasarPalabrasAMinusculas listaAComparar)) then { pelicula | matchPercentage = pelicula.matchPercentage + porcentaje }
                                                                           else pelicula
compararActor : Preferences -> Movie -> Movie
compararActor preferencias pelicula = comparacionConPreferencia 50 pelicula.actors preferencias.favoriteActor pelicula

compararGenero : Preferences -> Movie -> Movie
compararGenero preferencias pelicula = comparacionConPreferencia 60 pelicula.genre preferencias.genre pelicula

compararTitulo : Preferences -> Movie -> Movie
compararTitulo preferencias pelicula = foldr (comparacionConPreferencia 20 (dividirPalabrasPorEspacios pelicula.title)) pelicula (dividirPalabrasPorEspacios preferencias.keywords)

maximoCien : Movie -> Movie --ex funcionDeAzul :c
maximoCien pelicula = if pelicula.matchPercentage > 100 then {pelicula | matchPercentage = 100}
                         else pelicula

porcentajeDeCoincidencia : Preferences -> Movie -> Movie
porcentajeDeCoincidencia preferencias = maximoCien << (compararTitulo preferencias) << (compararGenero preferencias) << (compararActor preferencias)

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = (ordenarDescendientemente .matchPercentage) << (map ((porcentajeDeCoincidencia preferencias) << volverACalcular))
