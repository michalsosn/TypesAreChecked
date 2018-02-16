{-#LANGUAGE GADTs, TypeFamilies, DataKinds, PolyKinds, EmptyDataDecls, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}

module Proven.Basic where

data TBool = TFalse | TTrue
data SBool (b :: TBool) where
    SFalse :: SBool TFalse
    STrue  :: SBool TTrue

toBool :: SBool b -> Bool
toBool SFalse = False
toBool STrue  = True

type family (a :: TBool) :|: (b :: TBool) :: TBool where
    TTrue  :|: b = TTrue
    TFalse :|: b = b

type family (a :: TBool) :&: (b :: TBool) :: TBool where
    TFalse :&: b = TFalse
    TTrue  :&: b = b

data TNat = TZero | TSucc TNat
data SNat (n :: TNat) where
    SZero :: SNat TZero
    SSucc :: SNat n -> SNat (TSucc n)

toInt :: SNat n -> Int
toInt SZero = 0
toInt (SSucc n) = 1 + toInt n

type family (a :: TNat) :+: (b :: TNat) :: TNat where
    TZero   :+: b = b
    TSucc n :+: b = TSucc (n :+: b)

type family (a :: TNat) :*: (b :: TNat) :: TNat where
    TZero   :*: a = TZero
    TSucc n :*: b = b :+: (n :*: b)

type family IsZero (a :: TNat) :: TBool where
    IsZero TZero = TTrue
    IsZero a     = TFalse

type family IfTrue (t :: TBool) (a :: k) (b :: k) :: k where
    IfTrue TTrue  a b = a
    IfTrue TFalse a b = b

type T0 = TZero
type T1 = TSucc T0
type T2 = TSucc T1
type T3 = TSucc T2
type T4 = TSucc T3
type T5 = TSucc T4
type T6 = TSucc T5
type T7 = TSucc T6
type T8 = TSucc T7
type T9 = TSucc T8

class Sing (s :: k -> *) (t :: k) | t -> s where
    sing :: s t

instance Sing SBool TFalse where
    sing = SFalse
instance Sing SBool TTrue where
    sing = STrue

instance Sing SNat TZero where
    sing = SZero
instance (Sing SNat n) => Sing SNat (TSucc n) where
    sing = SSucc (sing :: SNat n)
