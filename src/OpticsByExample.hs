{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module OpticsByExample where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

doOpticsByExample :: String
doOpticsByExample = "OpticsByExample"

-- Chapter 3.2

-- q5
-- over :: Lens' s a -> (a -> a) -> s

-- _2 :: Lens' (a, b) -> a

-- (*10) :: Num -> Num
-- (*10) :: Num a => a -> a

-- (False, 2) :: (Bool, Num)

-- Chapter 3.3
data Ship = Ship
  { _name :: String,
    _numCrew :: Int
  }
  deriving (Show)

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
