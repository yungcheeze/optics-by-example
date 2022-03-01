{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter6.HigherOrderFolds where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T

-- 1.
{-
  *Chapter6.HigherOrderFolds> "Here's looking at you, kid" ^.. dropping 7 folded
  "looking at you, kid"
-}

{-
  *Chapter6.HigherOrderFolds> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (to words . folded)
  ["My","Hakuna","No"]
-}

{-
  *Chapter6.HigherOrderFolds> ["My Precious", "Hakuna Matata", "No problemo"] ^.. taking 1 folded . taking 2 folded
  "My"
-}

{-
  *Chapter6.HigherOrderFolds> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (to words . folded) . folded
  "MyHakunaNo"
-}

{-
  *Chapter6.HigherOrderFolds> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . takingWhile isAlpha folded
  "MyHakunaNo"
-}

{-
  *Chapter6.HigherOrderFolds> sumOf (taking 2 each) (10, 50, 100)
  60
-}

{-
  *Chapter6.HigherOrderFolds> ("stressed", "guns", "evil") ^.. backwards each
  ["evil","guns","stressed"]
-}

{-
  *Chapter6.HigherOrderFolds> ("stressed", "guns", "evil") ^.. backwards each . to reverse
  ["live","snug","desserts"]
-}

{-
  *Chapter6.HigherOrderFolds> "blink182 k9 blazeit420" ^.. to words . folded . droppingWhile isAlpha folded
  "1829420"
-}

-- 2.
sample :: [Int]
sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

measurementsTillFirstAboveZero = lengthOf (takingWhile (< 0) folded) sample

warmestInFirstFourDays = maximumOf (taking 4 folded) sample

dayAfterWarmestInFirstFourDays = sample ^? dropping 1 (droppingWhile (/= fromMaybe 0 warmestInFirstFourDays) folded)

consecutiveSubZeroDaysAtTheEnd = lengthOf (backwards (takingWhile (< 0) folded)) sample
