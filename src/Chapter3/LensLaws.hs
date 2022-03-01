{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter3.LensLaws where

import Control.Applicative
import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

type UserName = String

type UserId = String

data Session = Session
  { _userId :: UserId,
    _userName :: UserName,
    _createdTime :: String,
    _expiryTime :: String
  }
  deriving (Show, Eq)

makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

session = Session "user-1234" "Alfred Dunhill" "2019-07-25" "2019-08-25"
