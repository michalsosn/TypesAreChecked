{-# LANGUAGE BangPatterns #-}

module Strict.Strict where

newtype Strict a = Strict { runStrict :: a }
    deriving (Eq, Ord)

instance Functor Strict where
    fmap f (Strict a) = let !b = f a in Strict b

instance Applicative Strict where
    pure !a = Strict a
    (Strict !f) <*> (Strict !a) = let !b = f a in Strict b

instance Monad Strict where
    return !a = Strict a
    (Strict !a) >>= (!f) = let (Strict !b) = f a in Strict b
