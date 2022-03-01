{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter3.ShipSpec where

import Chapter3.Ship
import Control.Lens
import Test.Hspec
import Test.QuickCheck

newtype ValidShip = ValidShip Ship deriving (Eq, Show)

instance Arbitrary ValidShip where
  arbitrary = do
    n <- arbitrary
    c <- arbitrary
    return $ ValidShip $ Ship n c

spec :: Spec
spec = do
  describe "lens laws" $ do
    it "get-set" $
      conjoin [propGetSet name, propGetSet numCrew]

propSetGet :: Eq a => Lens' Ship a -> ValidShip -> a -> Bool
propSetGet l (ValidShip ship) newVal = view l (set l newVal ship) == newVal

propGetSet :: Lens' Ship a -> ValidShip -> Bool
propGetSet l (ValidShip s) = set l (view l s) s == s
