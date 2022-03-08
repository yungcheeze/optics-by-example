{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter6.FilteringFolds where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T

data Card = Card
  { _name :: String,
    _aura :: Aura,
    _holo :: Bool,
    _moves :: [Move]
  }
  deriving (Eq, Show)

data Aura
  = Wet
  | Hot
  | Spark
  | Leafy
  deriving (Eq, Show)

data Move = Move
  { _moveName :: String,
    _movePower :: Int
  }
  deriving (Eq, Show)

makeLenses ''Card
makeLenses ''Move

deck =
  [ Card "Skwortul" Wet False [Move "Squirt" 20],
    Card "Scorchander" Hot False [Move "Scorch" 20],
    Card "Seedasaur" Leafy False [Move "Allergize" 20],
    Card "Kapichu" Spark False [Move "Poke" 10, Move "Zap" 30],
    Card "Elecdude" Spark False [Move "Asplode" 50],
    Card "Garydose" Wet True [Move "Gary's move" 40],
    Card "Moisteon" Wet False [Move "Soggy" 3],
    Card "Grasseon" Leafy False [Move "Leaf Cut" 30],
    Card "Spicyeon" Hot False [Move "Capsaicisize" 40],
    Card "Sparkeon" Spark True [Move "Shock" 40, Move "Battery" 50]
  ]

allCardsThatStartWithS = deck ^.. folded . filteredBy (name . filtered ((== 'S') . head))

lowestAttackPowerofallmoves = minimumOf (folded . moves . folded . movePower) deck

-- DONE: Challenge 1 -- can I create a monoid so I get the card with
-- lowest attack power in a single pass?
data Minimum = Minimum
  { getMinimum :: Maybe Card
  }
  deriving (Show)

lowestAttackPower = minimumOf (moves . folded . movePower)

instance Monoid Minimum where
  mempty = Minimum Nothing
  mappend m1@(Minimum (Just c1)) m2@(Minimum (Just c2))
    | lowestAttackPower c1 < lowestAttackPower c2 = m1
    | otherwise = m2
  mappend m1@(Minimum (Just c1)) (Minimum Nothing) = m1
  mappend (Minimum Nothing) m2@(Minimum (Just c2)) = m2
  mappend _ _ = mempty

instance Semigroup Minimum where
  (<>) = mappend

cardWithLowestAttackPower = getMinimum $ foldOf (folded . to (Minimum . Just)) deck

-- TODO: Challenge 2 -- Can I make my Minimum monoid generic?
-- We'll need:
-- 1. a type
-- 2. a comparator
-- 3. make the type opaque
-- 4. add a getter function
--

hotCardsWithMoreThan30AttackPower =
  isJust $
    deck
      ^? folded
        . filteredBy (aura . only Hot)
        . filteredBy (moves . folded . movePower . filtered (> 30))

hotCardsWithMoreThan30AttackPower' =
  anyOf
    ( folded
        . filteredBy (aura . only Hot)
        . moves
        . folded
        . movePower
    )
    (> 30)
    deck

firstCardWithMoreThanOneMove =
  head $
    deck
      ^.. folded
        . filteredBy (moves . filtered ((> 1) . length))

allHolographicCardsWithWetAura =
  deck
    ^.. folded
      . filtered _holo
      . filteredBy (aura . only Wet)
      . name

sumOfAttackPowerForNonLeafyCards =
  sumOf
    ( folded
        . filteredBy (aura . filtered (/= Leafy))
        . moves
        . folded
        . movePower
    )
    deck
