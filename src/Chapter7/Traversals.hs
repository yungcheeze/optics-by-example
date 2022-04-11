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

{-
  ("N/A", "N/A")
-}

q2 = ("Jurassic", "Park") & both . traversed .~ 'x'

{-
  ("xxxxxxxx", "xxxx")
-}

q3 =
  ("Malcolm", ["Kaylee", "Inara", "Jayne"])
    & beside id traversed %~ take 3

{-
  ("Mal", ["Kay", "Ina", "Jay"])
-}

q4 = ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . element 1 .~ "River"

q4' = ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & elementOf (beside id traversed) 2 .~ "River"

{-
  ("Malcolm", ["Kaylee", "River", "Jayne"])
-}

q5 = ["Die Another Day", "Live and Let Die", "You Only Live Twice"] & traversed . elementOf worded 1 . traversed .~ 'x'

{-
  [ "Die xxxxxxx Day" , "Live xxx Let Die" , "You xxxx Live Twice" ]
-}

q6 = ((1, 2), (3, 4)) & beside both both +~ 1

{-
  ((2, 3), (4, 5))
-}

q7 = (1, (2, [3, 4])) & beside id (beside id each) +~ 1

{-
  (2, (3, [4, 5]))
-}

q8 = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each . filteredBy (_1 . filtered id) . _2 . taking 5 traversed %~ toUpper

q8' = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each . filtered fst . _2 . taking 5 traversed %~ toUpper

{-
  ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))
-}

q9 = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each %~ snd

{-
  ("Strawberries", "Blueberries", "Blackberries")
-}
