{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Jusion.Hash (Hash, hash) where

import Data.Hashable qualified as H
import GHC.Generics

-- NOTE, we are not using a cryptographically safe Hash
newtype Hash a = Hash Int
    deriving (Eq, Show, Generic)

instance (H.Hashable a) => H.Hashable (Hash a)

hash :: (H.Hashable a) => a -> Hash a
hash = Hash . H.hash