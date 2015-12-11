{-#LANGUAGE GADTs, TypeFamilies, DataKinds, PolyKinds, EmptyDataDecls, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables, TypeOperators #-}

module Proven.Basic where

import GHC.Exts (Constraint)
import Proven.Proven

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

toInt :: SNat n -> Int
toInt SZero = 0
toInt (SSucc n) = 1 + toInt n

type family (a :: TNat) :+: (b :: TNat) :: TNat where
    TZero   :+: b = b
    TSucc n :+: b = TSucc (n :+: b)

type family IsZero (a :: TNat) :: TBool where
    IsZero TZero = TTrue
    IsZero n     = TFalse

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


type family Concat (as :: [k]) (bs :: [k]) :: [k] where
    Concat '[]       bs = bs
    Concat (a ': tl) bs = a ': (Concat tl bs)

type family Elem (a :: k) (as :: [k]) :: Constraint where
    Elem a (a ': tl) = ()
    Elem a (b ': tl) = a `Elem` tl


data NonEmpty
instance Property NonEmpty [a] where
    check [] = Nothing
    check xs = Just (Verified xs)

data LengthMin (min :: TNat)
instance (Sing SNat min) => Property (LengthMin min) [a] where
    check xs
        | length xs >= toInt (sing :: SNat min) = Just (Verified xs)
        | otherwise = Nothing

data LengthMax (max :: TNat)
instance (Sing SNat max) => Property (LengthMax max) [a] where
    check xs
        | length xs <= toInt (sing :: SNat max) = Just (Verified xs)
        | otherwise = Nothing

-- Fajnie byłoby jakos wyrazić, że LengthMin 1 => NonEmpty oraz LengthMin 3 => LengthMin 2. Klasa Proven zamiast Verified?

safeHead :: Verified NonEmpty [a] -> a
safeHead = head . unVerified

safeTail :: Verified NonEmpty [a] -> [a]
safeTail = tail . unVerified

someSum :: Verified '[LengthMin T3, LengthMax T4] [Int] -> Int
someSum = sum . unVerified

demo :: IO ()
demo = do
    print $ fmap safeHead $ check [1, 2, 3]
    print $ safeHead <?> [1, 23, 46]
    print $ safeTail <?> [1, 23, 46]
    print $ fmap safeHead $ check ([] :: [()])
    print $ someSum <?> [2..3]
    print $ someSum <?> [2..4]
    print $ someSum <?> [2..5]
    print $ someSum <?> [2..6]

--    let smieszek = check [1, 3, 5] :: Maybe (Verified (LengthMin T2) [Int])
--    print $ fmap safeHead smieszek

