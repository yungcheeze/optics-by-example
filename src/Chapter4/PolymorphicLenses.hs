{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter4.PolymorphicLenses (Promotion, item) where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Promotion a = Promotion
  { _item :: a,
    _discountPercentage :: Double
  }
  deriving (Show)

item :: Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter :: Promotion a -> a
    getter = _item
    setter :: Promotion a -> b -> Promotion b
    setter promo newItem = promo {_item = newItem}

p = Promotion "Food" 0.25
