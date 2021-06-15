module TiposDeDatos where

--Parte 1: Modelado

data Direccion = Norte | Sur | Este | Oeste deriving (Show, Eq)

data Bolita = Rojo | Azul | Verde | Negro deriving (Show, Eq)

data Celda = Celda
  { posicion :: (Int, Int), -- Posicion cardinal de la celda
    bolitas :: [Bolita] -- Lista de las bolitas que hay en esa celda
  }
  deriving (Show)

type Cabezal = (Int, Int)

data Tablero = Tablero
  { dimensionX :: Int,
    dimensionY :: Int,
    cabezal :: Cabezal,
    celdas :: [Celda]
  }
  deriving (Show)

type Sentencia = Tablero -> Tablero

type Condicion = Tablero -> Bool