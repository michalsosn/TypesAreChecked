{-#LANGUAGE TypeFamilies, DataKinds, PolyKinds, FunctionalDependencies, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Proven.Proven where

import Data.Maybe

newtype Verified p a = Verified { unVerified :: a }

class Property p a where
    check     :: a -> Maybe (Verified p a)

instance Property '[] a where
    check = Just . Verified

instance (Property p1 a, Property ps a) => Property (p1 ': ps) a where
    check a = do
        check a :: Maybe (Verified p1 a)
        check a :: Maybe (Verified ps a)
        return (Verified a)

(<?>) :: (Property p a) => (Verified p a -> b) -> a -> Maybe b
f <?> a = f `fmap` (check a)



