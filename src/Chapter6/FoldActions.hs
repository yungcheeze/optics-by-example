{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter6.FoldActions where

import Control.Applicative
import Control.Lens
import Data.Char
import Data.Foldable
import Data.Function (on)
import qualified Data.Map as M
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text as T

data Actor = Actor
  { _name :: String,
    _birthYear :: Int
  }
  deriving (Show, Eq)

makeLenses ''Actor

data TVShow = TVShow
  { _title :: String,
    _numEpisodes :: Int,
    _numSeasons :: Int,
    _criticScore :: Double,
    _actors :: [Actor]
  }
  deriving (Show, Eq)

makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother =
  TVShow
    { _title = "How I Met Your Mother",
      _numEpisodes = 208,
      _numSeasons = 9,
      _criticScore = 83,
      _actors =
        [ Actor "Josh Radnor" 1974,
          Actor "Cobie Smulders" 1982,
          Actor "Neil Patrick Harris" 1973,
          Actor "Alyson Hannigan" 1974,
          Actor "Jason Segel" 1980
        ]
    }

buffy :: TVShow
buffy =
  TVShow
    { _title = "Buffy the Vampire Slayer",
      _numEpisodes = 144,
      _numSeasons = 7,
      _criticScore = 81,
      _actors =
        [ Actor "Sarah Michelle Gellar" 1977,
          Actor "Alyson Hannigan" 1974,
          Actor "Nicholas Brendon" 1971,
          Actor "David Boreanaz" 1969,
          Actor "Anthony Head" 1954
        ]
    }

tvShows :: [TVShow]
tvShows = [howIMetYourMother, buffy]

totalEpisodes = sumOf (folded . numEpisodes) tvShows

highestCriticScore = maximumOf (folded . criticScore) tvShows

oldestActor = minimumByOf (folded . actors . folded) (comparing _birthYear) tvShows

comparingOf :: Ord a => Lens' s a -> s -> s -> Ordering
comparingOf l = comparing (view l)

oldestActor' = minimumByOf (folded . actors . folded) (comparingOf birthYear) tvShows

calcAge :: Actor -> Int
calcAge actor = 2030 - _birthYear actor

showActor :: Actor -> String
showActor actor = _name actor <> ": " <> show (calcAge actor)

printAllActorAges = traverseOf_ (folded . actors . folded . to showActor) putStrLn tvShows

data AgeSummary = AgeSummary
  { _totalYears :: Sum Int,
    _numPeople :: Sum Int
  }
  deriving (Show)

instance Monoid AgeSummary where
  mempty = AgeSummary mempty mempty
  mappend s1 s2 = AgeSummary (foldMap _totalYears [s1, s2]) (foldMap _numPeople [s1, s2])

instance Semigroup AgeSummary where
  (<>) = mappend

ageSummary :: Actor -> AgeSummary
ageSummary actor = AgeSummary {_numPeople = Sum 1, _totalYears = Sum (calcAge actor)}

calcAverage :: AgeSummary -> Double
calcAverage (AgeSummary (Sum years) (Sum people)) = fromIntegral years / fromIntegral people

averageAge = calcAverage $ foldOf (folded . actors . folded . to ageSummary) tvShows

actorCount =
  foldMapByOf
    (folded . actors . folded . name)
    (M.unionWith (+))
    mempty
    (`M.singleton` 1)
    tvShows

actorCount' =
  foldByOf
    (folded . actors . folded . name . to (`M.singleton` 1))
    (M.unionWith (+))
    mempty
    tvShows

actorCount'' =
  foldrOf
    (folded . actors . folded . name)
    (\a m -> M.unionWith (+) (M.singleton a 1) m)
    mempty
    tvShows

actorCount''' =
  foldlOf
    (folded . actors . folded . name)
    (\m a -> M.unionWith (+) (M.singleton a 1) m)
    mempty
    tvShows

-- Exercises
-- 1.
{-
  *Chapter6.FoldActions> has folded []
  False
-}

{-
  *Chapter6.FoldActions> foldOf both ("Yo", "Adrian!")
  "YoAdrian!"
-}

{-
  *Chapter6.FoldActions> elemOf each "phone" ("E.T.", "phone", "home")
  True
-}

{-
  *Chapter6.FoldActions> minimumOf folded [5, 7, 2, 3, 13, 17, 11]
  Just 2
-}

{-
  *Chapter6.FoldActions> lastOf folded [5, 7, 2, 3, 13, 17, 11]
  Just 11
-}

{-
  *Chapter6.FoldActions> anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
  True
-}

{-
  *Chapter6.FoldActions> findOf folded even [11, 22, 3, 5, 6]
  Just 22
-}

-- 2.
input1 = ["umbrella", "olives", "racecar", "hammer"]

output1 = findOf folded isPalindrome input1

isPalindrome :: [Char] -> Bool
isPalindrome xs = xs == reverse xs

--
input2 = (2, 4, 6)

output2 = allOf each even input2

--
input3 = [(2, "I'll"), (3, "Be"), (1, "Back")]

output3 = maximumByOf each cmp input3
  where
    cmp x y = compare (fst x) (fst y)

output3' = maximumByOf folded (compare `on` fst) input3

--
input4 = (1, 2)

output4 = sumOf each input4
