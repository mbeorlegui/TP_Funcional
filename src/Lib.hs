import Data.List
import FuncionesAuxiliares
import Text.Show.Functions
import TiposDeDatos

--Parte 2: Inicializar Tablero

inicializarTablero :: Int -> Int -> Tablero
inicializarTablero ancho alto
  | ancho <= 0 || alto <= 0 = error "No se puede crear un tablero con estas dimensiones"
  | otherwise = Tablero ancho alto (1, 1) (inicializarCeldas ancho alto)

inicializarCeldas :: Int -> Int -> [Celda]
inicializarCeldas ancho alto = concatMap (agregarAListaCeldas alto) [1 .. ancho]

agregarAListaCeldas :: Int -> Int -> [Celda]
agregarAListaCeldas alto ancho = map (crearCelda ancho) [1 .. alto]

crearCelda :: Int -> Int -> Celda
crearCelda ancho alto = Celda {posicion = (ancho, alto), bolitas = []}

--Parte 3: Sentencias primitivas del lenguaje

mover :: Direccion -> Sentencia
mover unaDireccion unTablero 
  | puedeMoverse unaDireccion unTablero = mapCabezal (moverCabezal unaDireccion) unTablero
  | otherwise = error "El cabezal se cayó del tablero"

mapCabezal :: (Cabezal -> Cabezal) -> Tablero -> Tablero
mapCabezal unaFuncion unTablero = unTablero { cabezal = unaFuncion . cabezal $ unTablero }

moverCabezal :: Direccion -> Cabezal -> Cabezal
moverCabezal unaDireccion (posX, posY)
  | unaDireccion == Este = (posX + 1, posY)
  | unaDireccion == Oeste = (posX - 1, posY)
  | unaDireccion == Norte = (posX, posY + 1)
  | unaDireccion == Sur = (posX, posY - 1)

poner :: Bolita -> Sentencia
poner unaBolita unTablero = mapCeldaActual (mapBolitas . ponerBolita $ unaBolita) unTablero

sacar :: Bolita -> Sentencia
sacar unaBolita unTablero
  | hayBolita unaBolita unTablero = mapCeldaActual (mapBolitas . delete $ unaBolita) unTablero
  | otherwise = error ("No hay bolitas del color " ++ show unaBolita ++ " para sacar de la celda actual")

mapCeldaActual :: (Celda -> Celda) -> Tablero -> Tablero
mapCeldaActual unaFuncion unTablero = unTablero { celdas = (unaFuncion . celdaActual) unTablero : celdasSinCeldaActual (cabezal unTablero) (celdas unTablero) }

mapBolitas :: ([Bolita] -> [Bolita]) -> Celda -> Celda
mapBolitas unaFuncion unaCelda = unaCelda { bolitas = (unaFuncion . bolitas) unaCelda }

ponerBolita :: Bolita -> [Bolita] -> [Bolita]
ponerBolita unaBolita unasBolitas = unaBolita : unasBolitas

--Parte 4: Sentencias compuestas del lenguaje

repetir :: Int -> [Sentencia] -> Sentencia
repetir cantidadDeVeces unasSentencias unTablero
  | cantidadDeVeces <= 0 = unTablero
  | otherwise = repetir (cantidadDeVeces - 1) unasSentencias (programa unasSentencias unTablero)

alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Sentencia
alternativa unaCondicion unasSentencias1 unasSentencias2 unTablero
  | unaCondicion unTablero = programa unasSentencias1 unTablero
  | otherwise = programa unasSentencias2 unTablero

si :: Condicion -> [Sentencia] -> Sentencia
si unaCondicion unasSentencias = alternativa unaCondicion unasSentencias []

siNo :: Condicion -> [Sentencia] -> Sentencia
siNo unaCondicion = si (not . unaCondicion)

mientras :: Condicion -> [Sentencia] -> Sentencia
mientras unaCondicion unasSentencias unTablero
  | unaCondicion unTablero = mientras unaCondicion unasSentencias (programa unasSentencias unTablero)
  | otherwise = unTablero

irAlBorde :: Direccion -> Sentencia
irAlBorde unaDireccion = mientras (puedeMoverse unaDireccion) [mover unaDireccion]

--Parte 5: Condiciones

puedeMoverse :: Direccion -> Condicion
puedeMoverse unaDireccion unTablero =
  sePuedeMoverElCabezal unaDireccion (cabezal unTablero) (dimensionX unTablero) (dimensionY unTablero)

sePuedeMoverElCabezal :: Direccion -> Cabezal -> Int -> Int -> Bool 
sePuedeMoverElCabezal Este (posicionX, _) dimensionX _ = posicionX < dimensionX
sePuedeMoverElCabezal Oeste (posicionX, _) _ _ = posicionX > 1
sePuedeMoverElCabezal Norte (_, posicionY) _ dimensionY = posicionY < dimensionY
sePuedeMoverElCabezal Sur (_, posicionY) _ _ = posicionY > 1

hayBolita :: Bolita -> Condicion 
hayBolita unaBolita = elem unaBolita . bolitasDeCelda

cantidadDeBolitas :: Bolita -> Tablero -> Int
cantidadDeBolitas unaBolita = length . filter (== unaBolita) . bolitasDeCelda

-- Parte 6: programa
programa :: [Sentencia] -> Sentencia
programa unasSentencias unTablero = foldr ($) unTablero (reverse unasSentencias)

-- Parte 7: Bloque de codigo
-- Utilizar tableroDePruebaVacio para las pruebas de este punto

bloqueDeCodigo :: [Sentencia]
bloqueDeCodigo =
  primerBloque ++ repeticionPor15 ++ ifElse ++ [mover Este] ++ while ++ [poner Azul]

primerBloque :: [Sentencia]
primerBloque =
  [ mover Norte,
    poner Negro,
    poner Negro,
    poner Azul,
    mover Norte
  ]

repeticionPor15 :: [Sentencia]
repeticionPor15 = [repetir 15 [poner Rojo, poner Azul]]

ifElse :: [Sentencia]
ifElse = [alternativa (hayBolita Verde) sentenciaTrue sentenciaFalse]

sentenciaTrue :: [Sentencia]
sentenciaTrue = [mover Este, poner Negro]

sentenciaFalse :: [Sentencia]
sentenciaFalse = [mover Sur, mover Este, poner Azul]

while :: [Sentencia]
while = [mientras ((<= 9) . cantidadDeBolitas Verde) [poner Verde]]
