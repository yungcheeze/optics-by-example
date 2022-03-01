{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter3.IsItALens where

import Control.Applicative
import Control.Lens
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

getSecond :: (a, b, c) -> b
getSecond (x, y, z) = y

setSecond :: (a, b, c) -> b -> (a, b, c)
setSecond (x, y, z) y' = (x, y', z)

second :: Lens' (a, b, c) b
second = lens getSecond setSecond

chosen :: Lens' (Either a a) a
chosen = lens getter setter
  where
    getter (Left a) = a
    getter (Right a) = a
    setter (Left a) a' = Left a'
    setter (Right a) a' = Right a'

conditional :: Lens' (Bool, a, a) a
conditional = lens getter setter
  where
    getter (True, x, y) = x
    getter (False, x, y) = y
    setter (True, x, y) x' = (True, x', y)
    setter (False, x, y) y' = (False, y', y)
