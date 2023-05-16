{-# LANGUAGE TemplateHaskell #-}
module Jusion.Blockchain where

import Data.Map.Strict as Map

import Control.Lens.TH

import Jusion.Common
import Jusion.Hash

data Blockchain block = Blockchain
    { _byHeight :: Map.Map Height block
    , _byHash :: Map.Map (Hash block) block
    }
    deriving (Eq, Show)

makeLenses ''Blockchain