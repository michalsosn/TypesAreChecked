{-# LANGUAGE TypeFamilies #-}

module Hungry where

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


newtype Accum = Accum { unAccum :: Int }

instance Hungry Accum where
    type Bit Accum = Int
    eat acc n = Accum $ unAccum acc + n

instance Process Accum where
    type Input Accum = Int
    type Result Accum = Int
    run acc n = let sum = unAccum acc + n in (sum, Accum sum)

demo :: IO ()
demo = do
    let bigBoy = Accum 0
    print $ unAccum bigBoy
    print $ unAccum $ bigBoy `eat` 10 `eat` 11 `eat` 14




