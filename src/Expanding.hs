{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds, DataKinds, KindSignatures, UndecidableInstances #-}

module Expanding where

type family (r1 :: [k]) :++ (r2 :: [k]) :: [k] where
    '[] :++ bs  = bs
    as  :++ '[] = as
    (a ': as) :++ bs = a ': (as :++ bs)

type family Drop (r1 :: [k]) (r2 :: [k]) :: [k] where
    Drop '[] bs  = bs
    Drop (a ': as) (a ': bs) = Drop as bs

data HList (r :: [*]) where
    HNull :: HList '[]
    HCons :: a -> HList r -> HList (a ': r)

hSing :: a -> HList '[a]
hSing a = HCons a HNull

data Runnable (r :: [*]) (b :: *) where
    RNull :: a -> Runnable '[] a
    RCons :: (r -> a -> b) -> Runnable rs a -> Runnable (r ': rs) b

--runWith :: Runnable r a -> HList r -> a
--runWith (RNull a) HNull = a
--runWith (RCons q pr) (HCons r rs) a = runWith nr rs (q r a)
--
--pullArg :: Runnable '[r] a r
--pullArg = RCons (\r _ -> r) (RNull id)

--instance Functor (Runnable rs a) where
--    fmap f (RNull g) = RNull (f . g)
--    fmap f (RCons q nr) = RCons q (fmap f nr)

--(->>=) :: Runnable rs1 a b -> (b -> Runnable rs2 b c) -> Runnable (rs1 :++ rs2) a c
--(RNull g)    ->>= f = f
--(RCons pr q) ->>= f = pr ->>= \pa ->

--prependArg :: Runnable '[r1] (Runnable rs2 b) -> Runnable (rs2 :++ '[r1]) b
--prependArg (RCons (RNull a) q) = RCons (RNull a) (\r1 x -> case q r1 x of
--                                                            RNull b -> b
--                                                            RCons pr p ->

--rjoin :: Runnable rs1 (Runnable rs2 b) -> Runnable (rs2 :++ rs1) b
--rjoin (RNull r) = r
--rjoin (RCons (RNull r) q) = r
--rjoin (RCons pr q) = rjoin (fmap (\x -> ) pr)

--(->>=) :: Runnable rs1 a -> (a -> Runnable rs2 b) -> Runnable (rs2 :++ rs1) b
--(RNull a)    ->>= f = f a
--(RCons pr q) ->>= f = pr ->>= \pa ->

--prependArg :: (a -> Runnable rs2 b) -> (r1 -> pa -> a) -> pa -> Runnable (rs2 :++ '[r1]) b
--prependArg mkRs2 q pa = let fst = RCons (RNull pa) (\r1 pa -> q r1 pa) in

--(->>) :: Runnable r1 a -> Runnable r2 b -> Runnable (r2 :++ r1) b
--m1 ->> m2 = m1 ->>= \_ -> m2


demo :: IO ()
demo = do
    print 111

{-
class TMonoid (a :: k) (b :: k)

type family Rev (rs :: [k]) :: [k] where
    Rev '[] = '[]
    Rev (a ': as) = Rev as :++ '[a]
-}
{-
newtype Pierdu a = Pierdu { rozpierdu :: a }

instance Functor Pierdu where
    fmap f = Pierdu . f . rozpierdu

instance Monad Pierdu where
    return = Pierdu
    m >>= f = f $ rozpierdu m

fmap   :: (a -> b) -> f a -> f b
return :: a -> m a
bind   :: m a -> (a -> m b) -> m b
-}