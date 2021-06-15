module FuncionesAuxiliares where

import TiposDeDatos

celdaActual :: Tablero -> Celda
celdaActual unTablero = head . filter (esCeldaActual . cabezal $ unTablero) . celdas $ unTablero

esCeldaActual :: Cabezal -> Celda -> Bool
esCeldaActual (posicionX, posicionY) unaCelda = (fst . posicion $ unaCelda) == posicionX && (snd . posicion $ unaCelda) == posicionY

celdasSinCeldaActual :: Cabezal -> [Celda] -> [Celda]
celdasSinCeldaActual elCabezal = filter (not . esCeldaActual elCabezal)

bolitasDeCelda :: Tablero -> [Bolita]
bolitasDeCelda unTablero = bolitas . celdaActual $ unTablero

listaDeCeldasDePrueba :: [Celda]
listaDeCeldasDePrueba =
  [ Celda {posicion = (2, 2), bolitas = [Rojo]},
    Celda {posicion = (2, 1), bolitas = [Verde, Negro]},
    Celda {posicion = (1, 2), bolitas = [Azul, Verde]},
    Celda {posicion = (1, 1), bolitas = [Negro]}
  ]

tableroDePrueba :: Tablero
tableroDePrueba =
  Tablero
    { dimensionX = 3,
      dimensionY = 3,
      cabezal = (1, 2),
      celdas =
        [ Celda {posicion = (3, 3), bolitas = []},
          Celda {posicion = (3, 2), bolitas = []},
          Celda {posicion = (3, 1), bolitas = [Azul, Verde]},
          Celda {posicion = (2, 3), bolitas = []},
          Celda {posicion = (2, 2), bolitas = [Azul]},
          Celda {posicion = (2, 1), bolitas = [Negro, Rojo, Negro, Azul, Rojo]},
          Celda {posicion = (1, 3), bolitas = []},
          Celda {posicion = (1, 2), bolitas = []},
          Celda {posicion = (1, 1), bolitas = []}
        ]
    }

tableroDePruebaVacio :: Tablero
tableroDePruebaVacio =
  Tablero
    { dimensionX = 3,
      dimensionY = 3,
      cabezal = (1, 1),
      celdas =
        [ Celda {posicion = (3, 3), bolitas = []},
          Celda {posicion = (3, 2), bolitas = []},
          Celda {posicion = (3, 1), bolitas = []},
          Celda {posicion = (2, 3), bolitas = []},
          Celda {posicion = (2, 2), bolitas = []},
          Celda {posicion = (2, 1), bolitas = []},
          Celda {posicion = (1, 3), bolitas = []},
          Celda {posicion = (1, 2), bolitas = []},
          Celda {posicion = (1, 1), bolitas = []}
        ]
    }
