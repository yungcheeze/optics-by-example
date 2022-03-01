{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter5.LensOperators where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Payload = Payload
  { _weightKilos :: Int,
    _cargo :: String
  }
  deriving (Show)

makeLenses ''Payload

data Ship = Ship
  { _payload :: Payload
  }
  deriving (Show)

makeLenses ''Ship

s = Ship (Payload 3000 "Livestock")

-- 5.9
-- Q1
data Gate = Gate
  { _open :: Bool,
    _oilTemp :: Float
  }
  deriving (Show)

makeLenses ''Gate

data Army = Army
  { _archers :: Int,
    _knights :: Int
  }
  deriving (Show)

makeLenses ''Army

data Kingdom = Kingdom
  { _name :: String,
    _army :: Army,
    _gate :: Gate
  }
  deriving (Show)

makeLenses ''Kingdom

duloc :: Kingdom
duloc =
  Kingdom
    { _name = "Duloc",
      _army =
        Army
          { _archers = 22,
            _knights = 14
          },
      _gate =
        Gate
          { _open = True,
            _oilTemp = 10.0
          }
    }

goalA =
  duloc
    & name <>~ ": a perfect place"
    & army . knights +~ (42 - 14)
    & gate . open &&~ False

goalA' =
  duloc
    & (name <>~ ": a perfect place")
      . (army . knights +~ (42 - 14))
      . (gate . open &&~ False)

goalB =
  duloc
    & name <>~ "instein"
    & army . archers -~ (22 - 17)
    & army . knights -~ (14 - 26)
    & gate . oilTemp *~ 10

goalC =
  duloc
    & gate . oilTemp //~ 2
    & name <<>~ ": Home of the talking donkeys"
    & _1 %~ unwords . take 2 . words

goalC' =
  duloc
    & name <<>~ ": Home of the talking donkeys"
    & _1 %~ unwords . take 2 . words
    & _2 . gate . oilTemp //~ 2

-- 2
q2a = (False, "opposums") & _1 ||~ True

q2b = 2 & id *~ 3

q2c =
  ((True, "Dudley"), 55.0)
    & _1 . _2 <>~ " - the worst"
    & _2 -~ 15
    & _2 //~ 2
    & _1 . _2 %~ map toUpper
    & _1 . _1 &&~ False

-- 3. view

-- 4.
-- (%~) Lens s t a b -> (a -> b) -> s -> t
