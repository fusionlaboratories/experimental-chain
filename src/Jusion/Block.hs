{-# LANGUAGE TemplateHaskell #-}
module Jusion.Block where

import Control.Lens.TH

import Data.Hashable

import GHC.Generics ( Generic )

import Jusion.Common
import Jusion.Hash

data Block tx = Block
    { _parent :: Hash (Block tx)
    , _height :: Height
    , _transactions :: [tx]
    }
    deriving (Eq, Show, Generic)

makeLenses ''Block

instance (Hashable tx) => Hashable (Block tx)
