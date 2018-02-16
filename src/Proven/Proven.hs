{-#LANGUAGE TypeFamilies, PolyKinds #-}

module Proven.Proven where

import GHC.Exts (Constraint)

type family Concat (as :: [k]) (bs :: [k]) :: [k] where
    Concat '[]       bs = bs
    Concat (a ': tl) bs = a ': (Concat tl bs)

type family Elem (a :: k) (as :: [k]) :: Constraint where
    Elem a (a ': tl) = ()
    Elem a (b ': tl) = a `Elem` tl

type family Proven (Property p a)

