{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter3.Ship (Ship (..), name, numCrew) where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Ship = Ship
  { _name :: String,
    _numCrew :: Int
  }
  deriving (Show, Eq)

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew :: Ship -> Int -> Ship
setNumCrew ship newNumCrew = ship {_numCrew = newNumCrew}

numCrew :: Lens' Ship Int
numCrew = lens getNumCrew setNumCrew

getName :: Ship -> String
getName = _name

setName :: Ship -> String -> Ship
setName ship newName = ship {_name = newName}

name :: Lens' Ship String
name = lens getName setName

s = Ship {_numCrew = 10, _name = "BootyCall"}
