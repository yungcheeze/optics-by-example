{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter3.SelfCorrectingLenses (ProducePrices (..), limePrice, lemonPrice) where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data ProducePrices = ProducePrices
  { _limePrice :: Rational,
    _lemonPrice :: Rational
  }
  deriving (Show, Eq)

limePrice :: Lens' ProducePrices Rational
limePrice = lens getter setter
  where
    getter :: ProducePrices -> Rational
    getter = _limePrice
    setter :: ProducePrices -> Rational -> ProducePrices
    setter (ProducePrices lemonP _) p =
      ProducePrices
        { _limePrice = newPrice,
          _lemonPrice = clampWithin (newPrice, 0.5) lemonP
        }
      where
        newPrice = negativeToZero p

lemonPrice :: Lens' ProducePrices Rational
lemonPrice = lens getter setter
  where
    getter :: ProducePrices -> Rational
    getter = _lemonPrice
    setter :: ProducePrices -> Rational -> ProducePrices
    setter (ProducePrices _ limeP) p =
      ProducePrices
        { _limePrice = clampWithin (newPrice, 0.5) limeP,
          _lemonPrice = newPrice
        }
      where
        newPrice = negativeToZero p

negativeToZero :: Rational -> Rational
negativeToZero = max 0

clampWithin :: (Rational, Rational) -> Rational -> Rational
clampWithin (p, range) = max (p - range) . min (p + range)

p = ProducePrices 0.50 0.70
