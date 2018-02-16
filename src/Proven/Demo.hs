{-#LANGUAGE DataKinds, PolyKinds, EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}

module Proven.Demo where

import Proven.Basic
import Proven.Property

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
    someSum [1, 2, 3]
--    print $ fmap safeHead $ check [1, 2, 3]
--    print $ safeHead <?> [1, 23, 46]
--    print $ safeTail <?> [1, 23, 46]
--    print $ fmap safeHead $ check ([] :: [()])
--    print $ someSum <?> [2..3]
--    print $ someSum <?> [2..4]
--    print $ someSum <?> [2..5]
--    print $ someSum <?> [2..6]

--    let smieszek = check [1, 3, 5] :: Maybe (Verified (LengthMin T2) [Int])
--    print $ fmap safeHead smieszek

