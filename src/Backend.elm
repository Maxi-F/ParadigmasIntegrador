module Backend exposing(..)
import Models exposing(Movie, Preferences)
import List exposing(filter, map, sortBy, member, foldr, reverse)
import String exposing (contains, split)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = filter (peliculaTienePalabrasClave palabras)

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--
peliculaTienePalabrasClave palabras pelicula = contains "Toy" pelicula.title

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
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = completaAca

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = completaAca

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = completaAca

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

dividirPorEspacios : String -> List String
dividirPorEspacios = split " "

volverACalcular : Movie -> Movie
volverACalcular pelicula = {pelicula | matchPercentage = 0}

comparacionConPreferencia : Int -> List String -> String -> Movie -> Movie
comparacionConPreferencia porcentaje listaAComparar preferencia pelicula = if (member preferencia listaAComparar) then { pelicula | matchPercentage = pelicula.matchPercentage + porcentaje }
                                                                       else pelicula
compararActor : Preferences -> Movie -> Movie
compararActor preferencias pelicula = comparacionConPreferencia 50 pelicula.actors preferencias.favoriteActor pelicula

compararGenero : Preferences -> Movie -> Movie
compararGenero preferencias pelicula = comparacionConPreferencia 60 pelicula.genre preferencias.genre pelicula

compararTitulo : Preferences -> Movie -> Movie
compararTitulo preferencias pelicula = foldr (comparacionConPreferencia 20 (dividirPorEspacios pelicula.title)) pelicula (dividirPorEspacios preferencias.keywords)

funcionDeAzul : Movie -> Movie
funcionDeAzul pelicula = if pelicula.matchPercentage > 100 then {pelicula | matchPercentage = 100}
                         else pelicula

porcentajeDeCoincidencia : Preferences -> Movie -> Movie
porcentajeDeCoincidencia preferencias = funcionDeAzul << (compararTitulo preferencias) << (compararGenero preferencias) << (compararActor preferencias)

ordenarDescendientemente : (Movie -> Int) -> List Movie -> List Movie
ordenarDescendientemente criterio = reverse << (sortBy criterio)

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = (ordenarDescendientemente .matchPercentage) << (map ((porcentajeDeCoincidencia preferencias) << volverACalcular))
