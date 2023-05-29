module Jusion.Oracle where

import Control.Monad.Reader

import Data.Map.Strict qualified as Map
import Data.IntSet qualified as IntSet

import Jusion.Common
import Jusion.Hash ( Hash )

-- TODO: What if this was just a bloom filter based on block hashes?
-- many APIs and services actually assume that a block hash is unique...  What
-- is the possibility of collision?
data Blocks block = Blocks
    { _byHeight :: Map.Map Height (Hash block)
    , _hashes :: IntSet.IntSet
    }
newtype L1 block = L1 { blocks :: Map.Map Height (Hash block) }
    deriving (Eq, Show)

-- Here are the operations directly on the L1 Oracle
observeBlock :: MonadReader (L1 block) m => Height -> Hash block -> m ()
observeBlock = undefined

