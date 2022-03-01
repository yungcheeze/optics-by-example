{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter6.FoldsIntro where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Role
  = Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving (Show, Eq, Ord)

data CrewMember = CrewMember
  { _name :: String,
    _role :: Role,
    _talents :: [String]
  }
  deriving (Show, Eq, Ord)

makeLenses ''CrewMember

roster :: S.Set CrewMember
roster =
  S.fromList
    [ CrewMember "Grumpy Roger" Gunner ["Juggling", "Arbitrage"],
      CrewMember "Long-John Bronze" PowderMonkey ["Origami"],
      CrewMember "Salty Steve" PowderMonkey ["Charcuterie"],
      CrewMember "One-eyed Jack" Navigator []
    ]

crewMembers :: Fold (S.Set CrewMember) CrewMember
crewMembers = folded

crewRole :: Fold CrewMember Role
crewRole = role

rosterRoles :: Fold (S.Set CrewMember) Role
rosterRoles = crewMembers . crewRole

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

{-
>>> beastSizes ^.. folded
[(3,"Sirens"),(882,"Kraken"),(92,"Ogopogo")]
-}

{-
>>> beastSizes ^.. folded . folded
["Sirens","Kraken","Ogopogo"]
-}

{-
>>> beastSizes ^.. folded . folded . folded
"SirensKrakenOgopogo"
-}

{-
>>> toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
>>> [[1, 2, 3], [4, 5, 6]] ^.. folded . folded
[1, 2, 3, 4, 5, 6]
-}

quotes :: [(String, String, String)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

b = [(1, 'a'), (2, 'b'), (3, 'c')] ^.. folded . _1

-- _1 :: Int
-- folded :: Fold ([(Int, Char)])  [(Int, Char)]
