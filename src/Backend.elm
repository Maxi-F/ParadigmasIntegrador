--         _______________________________________________________________________
--        |:..                          TP  FUNCIONAL                ``:::%%%%%%%%|
--        |%%%:::::..                     PdeP-Flix                     `:::::%%%%|
--        |%%%%%%%:::::.....________________________________________________::::::|
--
--
--                           ___________
--                          [___________]
--                           {=========}
--                         .-'         '-.
--                        /               \
--                       /_________________\        
--                       |   _  _   _      |         A puras de esas anduvo el enfermo...
--                       ||\(_ |_)||_)||\ ||             y probablemente todavía anda
--      ,.--.   ,.--.    ||~\_)|  || \|| \||
--     // \  \ // \  \   |_________________|
--     \\  \ / \\  \ /   |                 |
--      `'--'   `'--'    '-----------------'
--
--

module Backend exposing(..)
import Models exposing(Movie, Preferences)
import List exposing(filter, map, sortBy, member, foldr, reverse, any, all)
import String exposing (contains, split, toLower, join)
import Tuple exposing (first, second)

completaAca = identity

abecedarioYNumeros : List Char
abecedarioYNumeros = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

generosRelacionados : List (String, String)
generosRelacionados = [("Action", "Adventure"), ("Action", "Superhero"), ("Suspense", "Horror")]

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

hacerComparableComoLista : String -> List String
hacerComparableComoLista = filtrarCaracteresEspeciales << pasarPalabrasAMinusculas << dividirPalabrasPorEspacios

hacerComparableComoString : String -> String
hacerComparableComoString = join " " << hacerComparableComoLista

hacerListaDeStringsComparable : List String -> List String
hacerListaDeStringsComparable = map hacerComparableComoString

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
esCoincidenciaClave titulo palabra = if palabra /= "" then contains palabra titulo
                                     else True

peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula = all (esCoincidenciaClave (hacerComparableComoString pelicula.title)) (hacerComparableComoLista palabras)

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero peliculas = if (genero /= "Genre" || genero /= "") then filter (esPeliculaDeGenero genero << .genre) peliculas
                                             else peliculas

esPeliculaDeGenero : String -> List String -> Bool
esPeliculaDeGenero genero generosDePelicula = member genero generosDePelicula

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores peliculas = if mostrarSoloMenores then soloParaMenores peliculas
                                                                else peliculas

soloParaMenores : List Movie -> List Movie
soloParaMenores = filter .forKids

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

perteneceARelacion : String -> ((String, String) -> String) -> (String, String) -> Bool
perteneceARelacion genero primeroOSegundo relacion = genero == (primeroOSegundo relacion)

buscarRelacionados : String -> (String, String) -> Bool
buscarRelacionados genero relacion = (perteneceARelacion genero first relacion) || (perteneceARelacion genero second relacion)

generoRelacionadoCon : String -> (String, String) -> String
generoRelacionadoCon genero relacion = if (perteneceARelacion genero first relacion) then second relacion
                                       else first relacion

generosConLosQueSeRelaciona : String -> List String
generosConLosQueSeRelaciona genero = ((map (generoRelacionadoCon genero)) << (filter (buscarRelacionados genero))) generosRelacionados

comparacionConPreferencia : Int -> List String -> String -> Movie -> Movie
comparacionConPreferencia porcentaje listaAComparar preferencia pelicula = if (member (hacerComparableComoString preferencia) (hacerListaDeStringsComparable listaAComparar)) then { pelicula | matchPercentage = pelicula.matchPercentage + porcentaje }
                                                                           else pelicula
compararActor : Preferences -> Movie -> Movie
compararActor preferencias pelicula = comparacionConPreferencia 50 pelicula.actors preferencias.favoriteActor pelicula

compararGenero : Preferences -> Movie -> Movie
compararGenero preferencias pelicula = comparacionConPreferencia 60 pelicula.genre preferencias.genre pelicula

compararTitulo : Preferences -> Movie -> Movie
compararTitulo preferencias pelicula = foldr (comparacionConPreferencia 20 (dividirPalabrasPorEspacios pelicula.title)) pelicula (dividirPalabrasPorEspacios preferencias.keywords)

compararGenerosRelacionados : Preferences -> Movie -> Movie
compararGenerosRelacionados preferencias pelicula = if pelicula.matchPercentage /= 60 then foldr (comparacionConPreferencia 15 pelicula.genre) pelicula (generosConLosQueSeRelaciona preferencias.genre) 
                                                    else pelicula

maximoCien : Movie -> Movie
maximoCien pelicula = if pelicula.matchPercentage > 100 then {pelicula | matchPercentage = 100}
                      else pelicula

porcentajeDeCoincidencia : Preferences -> Movie -> Movie
porcentajeDeCoincidencia preferencias = maximoCien << (compararTitulo preferencias) << (compararActor preferencias) << (compararGenerosRelacionados preferencias) << (compararGenero preferencias)

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = (ordenarDescendientemente .matchPercentage) << (map ((porcentajeDeCoincidencia preferencias) << volverACalcular))
