{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter7.Traversals where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

q1 = ("Jurassic", "Park") & beside id id .~ "N/A"

q1' = ("Jurassic", "Park") & both .~ "N/A"

q1'' = ("Jurassic", "Park") & each .~ "N/A"

q2 = ("Jurassic", "Park") & both . traversed .~ 'x'

q3 =
  ("Malcolm", ["Kaylee", "Inara", "Jayne"])
    & beside id traversed %~ take 3

q4 = ["Die Another Day", "Live and Let Die", "You Only Live Twice"] & traversed . elementOf worded 1 . traversed .~ 'x'

q5 = ((1, 2), (3, 4)) & beside both both +~ 1
