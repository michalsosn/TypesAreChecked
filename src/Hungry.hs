{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module Hungry where

import Control.Monad.State

class Hungry a where
    type Bit a
    eat :: a -> Bit a -> a

class Stream a where
    type Elem a
    next :: a -> (Elem a, a)

class Process a where
    type Input a
    type Result a
    run :: a -> Input a -> (Result a, a)

instance (Process p) => Hungry p where
    type Bit a = Input a
    eat p b = snd $ run p b

runProcess :: (MonadState p m, Process p) => Input p -> m (Result p)
runProcess = state . flip run


newtype Accum = Accum { unAccum :: Int }

instance Process Accum where
    type Input Accum = Int
    type Result Accum = Int
    run acc n = let sum = unAccum acc + n in (sum, Accum sum)

{-
instance Hungry Accum where
    type Bit Accum = Int
    eat acc n = Accum $ unAccum acc + n
-}


demo :: IO ()
demo = do
    let bigBoy = Accum 0
    print $ unAccum bigBoy
    print $ unAccum $ bigBoy `eat` 10 `eat` 11 `eat` 14




