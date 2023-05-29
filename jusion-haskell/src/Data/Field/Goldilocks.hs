{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Field.Goldilocks (
    Field (..),
    word8,
    word16,
    word32,
    word64,
    word128,
    word256,
    toWord8,
    toWord16,
    toWord32,
    toWord64,
    toWord128,
    toWord256,
) where

import Data.Aeson qualified as Aeson
import Data.FiniteField qualified as PrimeField (toInteger)
import Data.FiniteField.PrimeField (PrimeField)

import Data.Bits
import Data.Field
import Data.WideWord
import Data.Word
import Control.Applicative (Applicative(liftA2))


-- The Order of Miden's Goldilocks field is
-- >>> 2 ^ 64 - 2 ^ 32 + 1
-- 18446744069414584321
newtype Field = Field {unField :: PrimeField 18446744069414584321}
    deriving (Eq, Ord, Bounded, Enum, Num)

instance Show Field where
    show = show . unField

-- Encoding Field Elements as JSON Strings
instance Aeson.ToJSON Field where
    toJSON = Aeson.toJSON . show . unField

-- Various operators
word8 :: Word8 -> ToField Field
word8 x = [fromIntegral x]

word16 :: Word16 -> ToField Field
word16 x = [fromIntegral x]

word32 :: Word32 -> ToField Field
word32 x = [fromIntegral x]

-- TODO: this needs to be two word32
word64 :: Word64 -> ToField Field
word64 x = do
    -- get the upper part
    word32 $ fromIntegral $ shiftR x 32
    -- get the lower part
    word32 $ fromIntegral $ x .&. 0xFFFFFFFF

word128 :: Word128 -> ToField Field
word128 Word128{..} = do
    word64 word128Hi64
    word64 word128Lo64

word256 :: Word256 -> ToField Field
word256 Word256{..} = do
    word64 word256hi
    word64 word256m1
    word64 word256m0
    word64 word256lo

-- Unsafe, because it will get truncated
toNumUnsafe :: (Num a) => FromField Field a
toNumUnsafe = fmap (fromInteger . PrimeField.toInteger . unField) <$> takeField

toWord8 :: FromField Field Word8
toWord8 = toNumUnsafe

toWord16 :: FromField Field Word16
toWord16 = toNumUnsafe

toWord32 :: FromField Field Word32
toWord32 = toNumUnsafe

toWord64 :: FromField Field Word64
toWord64 = do
    maybeHi <- toWord32
    maybeLo <- toWord32
    pure $ do
        hi :: Word64 <- fromIntegral <$> maybeHi
        lo :: Word64 <- fromIntegral <$> maybeLo
        Just $ shiftL hi 32 + lo

toWord128 :: FromField Field Word128
toWord128 = liftA2 Word128 <$> toWord64 <*> toWord64

toWord256 :: FromField Field Word256
toWord256 = do
    maybeHi <- toWord64
    maybeM1 <- toWord64
    maybeM0 <- toWord64
    maybeLo <- toWord64
    pure $ do
        hi :: Word64 <- fromIntegral <$> maybeHi
        m1 :: Word64 <- fromIntegral <$> maybeM1
        m0 :: Word64 <- fromIntegral <$> maybeM0
        lo :: Word64 <- fromIntegral <$> maybeLo
        Just $ Word256 hi m1 m0 lo
    