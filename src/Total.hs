{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds, DataKinds, KindSignatures, UndecidableInstances, ScopedTypeVariables #-}

module Total where

import Data.Proxy

data Nat = Z | S Nat

data SBool = SFalse | STrue

class TypeInt a where
    toInt :: Proxy a -> Int

instance TypeInt Z where
    toInt _ = 0

instance (TypeInt n) => TypeInt (S n) where
    toInt _ = 1 + toInt (Proxy :: Proxy n)

type N1 = S Z
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4

type family (a :: Nat) :+ (b :: Nat) :: Nat where
    a :+ Z = a
    a :+ S pb = S (a :+ pb)

type family (a :: Nat) :* (b :: Nat) :: Nat where
    a :* Z = Z
    a :* S pb = a :+ (a :* pb)

type family (a :: Nat) :^ (b :: Nat) :: Nat where
    a :^ Z = S Z
    a :^ S pb = a :* (a :^ pb)

type family IsZero (a :: Nat) :: SBool where
    IsZero Z = STrue
    IsZero a = SFalse

--pkFactorial :: (TypeInt n) => (Proxy n -> Int) -> Proxy (S n) -> Int
--pkFactorial rec p = toInt p * rec Proxy
--
--myFoldi :: ((Proxy n -> a) -> Proxy (S n) a) -> Proxy n -> a
--myFoldi f = f (myFoldi f)

demo :: IO ()
demo = do
    print 100
    print $ toInt (Proxy :: Proxy (N2 :^ N3))
    print $ myFactorial 5
    putStrLn $ myFoldr (\n acc -> acc ++ " " ++ show n) "Nums:" [3..5]
    putStrLn $ myFoldn (++ "!") "Hello" 4
    putStrLn $ myFoldn' (++ "!") "Hello" 4
    print $ myFoldi mkFactorial 5
    print $ myFoldi' mkFactorial 5


mkFactorial :: (Int -> Int) -> Int -> Int
mkFactorial rec n = if n == 0 then 1 else n * rec (n - 1)

myFix :: (a -> a) -> a
myFix a = let b = a b in b

myFactorial :: Int -> Int
myFactorial = myFix mkFactorial

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _f z     [] = z
myFoldr  f z (a:as) = f a (myFoldr f z as)

myFoldn :: (a -> a) -> a -> Int -> a
myFoldn _f z 0 = z
myFoldn f z n = myFoldn f (f z) (n - 1)

myFoldn' :: (a -> a) -> a -> Int -> a
myFoldn' f z n = myFoldr (const f) z (replicate n ())

myFoldi :: (a -> a) -> a
myFoldi f = f (myFoldi f)

-- | This is fix! Requires undefined starting value and an infinite list?
myFoldi' :: (a -> a) -> a
myFoldi' f = myFoldr (const f) undefined (repeat ())


-- | Requires UndecidableInstances
{-
type family Diverge a where
    Diverge a = Diverge a

evilType :: Diverge Int
evilType = 3
-}
