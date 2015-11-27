{-#LANGUAGE RecursiveDo #-}

module Reccer where

import Control.Monad
import Control.Monad.Tardis
import Control.Monad.State
import Data.List

dupcia :: Int -> Int -> Maybe [Int]
dupcia a b = mdo
    guard (a /= b)
    xs <- Just (a : ys)
    ys <- Just (b : xs)
    return $ xs

kiermasz :: Int -> Int -> [[Int]]
kiermasz a b = mdo
    xs <- (fmap . fmap) (a*) [1:2:ys, 100:200:ys]
    ys <- (fmap . fmap) (b*) [10:xs, 1000:xs]
    [zipWith (+) xs ys]

kawalek :: Int -> Int -> [Int]
kawalek a b = mdo
    xs <- [1:2:take a xs, 4:5:take b xs]
    xs

primes :: [Int]
primes = 2 : sieve 3
    where
        sieve :: Int -> [Int]
        sieve n = if all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p < n) primes)
                  then n : sieve (n + 2)
                  else sieve (n + 2)

pierdek :: Int -> State [Int] [Int]
pierdek a = mdo
    xs  <- (fmap . fmap) (+y) $ state (splitAt a)
    [y] <- state (splitAt 1)
    return xs

przeszlosc :: Tardis Int String ()
przeszlosc = do
    name <- getPast
    sendFuture (name ++ " Burton")
    rec
      sendPast (score + 1)
      score <- getFuture
    return ()

circular :: [Int] -> ([Int], [Int])
circular [] = ([], [])
circular xs@(hd:_) =
    let (less, more, mean) = aux mean xs [] [] hd hd
    in  (less, more)
    where
        aux :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> ([Int], [Int], Int)
        aux mid [] less more minx maxx = (less, more, (minx + maxx) `div` 2)
        aux mid (x:xs) less more minx maxx =
            let less' = if x <= mid then x:less else less
                more' = if x <= mid then more else x:more
                minx' = min x minx
                maxx' = max x maxx
            in  aux mid xs less' more' minx' maxx'
-- WRONG! NOT | x <= mid  = aux mid xs (x:less) more (min x minx) maxx
--  CIRCULAR  | otherwise = aux mid xs less (x:more) minx (max x maxx)

demo :: IO ()
demo = do
    putStrLn "Start Reccer"
    print $ fmap (take 10) $ dupcia 2 2
    print $ fmap (take 10) $ dupcia 2 3
    print $ take 10 $ fmap (take 10) $ kiermasz 5 6
    print $ take 20 $ kawalek 5 10
    print $ take 50 primes
    print $ execTardis przeszlosc (10, "Dan")
    print $ circular [1, 2, 4, 6, 3, 9, 0, 8]
    print $ (\(a, b) -> (take 21 a, take 21 b)) $ flip runState [1..] $ pierdek 4
    putStrLn "End Reccer"

