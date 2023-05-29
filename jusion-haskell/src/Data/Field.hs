{-# LANGUAGE OverloadedLists #-}
module Data.Field where

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Data.DList qualified as DList
import Data.Typeable
import GHC.Exts qualified
import GHC.Generics
import GHC.List

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

-- FieldReader Interface
newtype FieldReader f a = FieldReader { runFieldReader :: State [f] a }
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance MonadState [f] (FieldReader f)

instance (a ~ ()) => GHC.Exts.IsList (FieldReader f a) where
  type Item (FieldReader f a) = f
  fromList = put
  toList = snd . flip runState mempty . runFieldReader

type FromField f a = FieldReader f (Maybe a)

takeField :: FromField f f
takeField = do
  s <- get
  case uncons s of
    Nothing -> pure Nothing
    Just (h, r) -> do
      put r
      pure $ Just h
