{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter3.VirtualFields where

import Control.Applicative
import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Temperature = Temperature
  { _location :: String,
    _celsius :: Float
  }
  deriving (Show, Eq)

makeLenses ''Temperature

farenheit :: Lens' Temperature Float
farenheit = lens getter setter
  where
    getter :: Temperature -> Float
    getter = celsiusToFarenheit . view celsius
    setter :: Temperature -> Float -> Temperature
    setter t f = set celsius (farenheitToCelsius f) t

celsiusToFarenheit :: Float -> Float
celsiusToFarenheit c = (c * (9 / 5)) + 32

farenheitToCelsius :: Float -> Float
farenheitToCelsius f = (f - 32) * (5 / 9)

t = Temperature "Vancouver" (-10)

-- Exercise
-- Q1
data User = User
  { _firstName :: String,
    _lastName :: String,
    _email :: String
  }
  deriving (Show, Eq)

makeLenses ''User

username :: Lens' User String
username = email

fullName :: Lens' User String
fullName = lens getter setter
  where
    getter :: User -> String
    getter u = view firstName u ++ " " ++ view lastName u
    setter :: User -> String -> User
    setter u newName = set l newName' u
      where
        newName' = span (/= ' ') newName
        l = lensProduct firstName lastName

u = User "Ucizi" "Mafeni" "ucizim@gmail.com"
