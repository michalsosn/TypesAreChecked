{-#LANGUAGE TypeFamilies, DataKinds, PolyKinds, FunctionalDependencies, ScopedTypeVariables, TypeOperators, FlexibleInstances #-}

module Proven.Property where

import Data.Maybe

class Property p a where
    check :: (Proven t p) => a -> f p a

instance Property '[] a where
    check = prove

instance (Property p1 a, Property ps a) => Property (p1 ': ps) a where
    check a = (check a :: f p1 a *> (check a :: f ps a) *> prove a


class Proven t p where
    type Context t
    prove :: a -> (Context t) f p a

newtype Verified p a = Verified { unVerified :: Maybe a }

(<?>) :: (Property p a) => (Verified p a -> b) -> a -> Maybe b
f <?> a = f `fmap` (check a)



