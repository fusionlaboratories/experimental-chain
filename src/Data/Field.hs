{-# LANGUAGE OverloadedLists #-}
module Data.Field where

import Control.Monad.Writer.Strict
import Data.DList qualified as DList
import Data.Typeable
import GHC.Exts qualified
import GHC.Generics

-- Field Writer Interface
newtype FieldWriter f a = FieldWriter { runFieldWriter :: Writer (DList.DList f) a }
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance MonadWriter (DList.DList f) (FieldWriter f)

instance (a ~ ()) => GHC.Exts.IsList (FieldWriter f a) where
    type Item (FieldWriter f a) = f
    fromList = tell . DList.fromList
    toList = DList.toList . snd . runWriter . runFieldWriter

type ToField f = FieldWriter f ()

enum :: (Enum a, Integral f) => a -> ToField f
enum a = [fromIntegral $ fromEnum a]
