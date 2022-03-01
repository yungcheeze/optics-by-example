{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter4.ComposingLenses where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Person = Person
  { _name :: String,
    _address :: Address
  }
  deriving (Show)

data Address = Address
  { _streetAddress :: StreetAddress,
    _city :: String,
    _country :: String
  }
  deriving (Show)

data StreetAddress = StreetAddress
  { _streetNumber :: String,
    _streetName :: String
  }
  deriving (Show)

makeLenses ''Person
makeLenses ''Address
makeLenses ''StreetAddress

sherlock :: Person
sherlock =
  Person
    { _name = "S. Holmes",
      _address =
        Address
          { _streetAddress =
              StreetAddress
                { _streetNumber = "221B",
                  _streetName = "Baker Street"
                },
            _city = "London",
            _country = "England"
          }
    }

setStreetNumber :: String -> Person -> Person
setStreetNumber newStreetNumber p@(Person _ a@(Address s@(StreetAddress streetNumber _) _ _)) = p {_address = a {_streetAddress = s {_streetNumber = newStreetNumber}}}

updateStreetNumber :: (String -> String) -> (StreetAddress -> StreetAddress)
updateStreetNumber modify existingStreetAddress =
  existingStreetAddress
    { _streetNumber = modify . _streetNumber $ existingStreetAddress
    }

updateStreetAddress :: (StreetAddress -> StreetAddress) -> (Address -> Address)
updateStreetAddress modify existingAddress =
  existingAddress
    { _streetAddress = modify . _streetAddress $ existingAddress
    }

-- view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))

-- fiveEightDomino :: Lens' Five Eight
-- mysteryDomino :: Lens' Eight Two
-- twoThreeDomino :: Lens' Two Three
-- dominoTrain :: Lens' Five Three
-- dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

-- Lens Armadillo Platypus Hedgehog BabySloth
