{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter6.CustomFolds where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

newtype Name = Name
  { getName :: String
  }
  deriving (Show, Eq)

data ShipCrew = ShipCrew
  { _shipName :: Name,
    _captain :: Name,
    _firstMate :: Name,
    _conscripts :: [Name]
  }
  deriving (Show)

makeLenses ''ShipCrew

crew = ShipCrew (Name "Bestina") (Name "Black Jack") (Name "Timmy") [Name "JoJo", Name "Foster"]

collectCrewMembers :: ShipCrew -> [Name]
collectCrewMembers crew = [_captain crew, _firstMate crew] <> _conscripts crew

crewMembers :: Fold ShipCrew Name
crewMembers = folding collectCrewMembers

{-
  *Chapter6.CustomFolds> crew ^.. crewMembers . to getName
  ["Black Jack","Timmy","JoJo","Foster"]
-}

crewNames :: Fold ShipCrew Name
crewNames = folding (\s -> s ^.. captain <> s ^.. firstMate <> s ^.. conscripts . folded)

-- 1
{-
  *Chapter6.CustomFolds> ["Yer", "a", "wizard", "Harry"] ^.. folded . folded
  "YerawizardHarry"
-}

{-
  *Chapter6.CustomFolds> [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
  [1,2,4,5]
-}

{-
  *Chapter6.CustomFolds> [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
  [[1,2],[4,5]]
-}

{-
  *Chapter6.CustomFolds> ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded
  "cbafed"
-}

-- 2
{-
  *Chapter6.CustomFolds> [1..5] ^.. folded . to (*100)
  [100,200,300,400,500]
-}

{-
  *Chapter6.CustomFolds> [1..5] ^.. folded . folding (\s -> [s * 100])
  [100,200,300,400,500]
-}

{-
  *Chapter6.CustomFolds> [(1, "one"), (2, "two")] ^.. folded . folded
  ["one","two"]
-}

{-
  *Chapter6.CustomFolds> (Just 1, Just 2, Just 3) ^.. each . each
  [1,2,3]
-}

takeRight :: Either a b -> [b]
takeRight (Right b) = [b]
takeRight _ = []

{-
  *Chapter6.CustomFolds> [Left 1, Right 2, Left 3] ^.. folded . folding takeRight
  [2]
-}

{-
  *Chapter6.CustomFolds> [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. each . each . each
  [1,2,3,4,5,6,7,8]
-}

leftOddRightEven :: Integral a => a -> Either a a
leftOddRightEven a
  | a `mod` 2 == 1 = Left a
  | otherwise = Right a

{-
  *Chapter6.CustomFolds> [1..4] ^.. folded . to leftOddRightEven
  [Left 1,Right 2,Left 3,Right 4]
-}

{-
  *Chapter6.CustomFolds> [(1, (2, 3)), (4, (5, 6))] ^.. folded . folding (\s -> s ^.. _1 <> s ^.. _2 . each)
  [1,2,3,4,5,6]
-}

dropNothing :: Maybe a -> [a]
dropNothing a = [x | Just x <- [a]]

{-
  *Chapter6.CustomFolds> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\s -> s ^.. _1 . folding dropNothing <> s ^.. _2 . folding takeRight)  [1,2]
  [1,2]
-}

{-
  *Chapter6.CustomFolds> [(1, "one"), (2, "two")] ^.. folded . folding (\s -> s ^.. _1 . to Left <> s ^.. _2 . to Right)
  [Left 1,Right "one",Left 2,Right "two"]
-}

{-
  *Chapter6.CustomFolds> S.fromList ["apricots", "apples"] ^.. folded . to reverse . folded
  "selppastocirpa"
-}
