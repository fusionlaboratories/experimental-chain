module Data.Field where

class ToField a b where
    toField :: a -> b