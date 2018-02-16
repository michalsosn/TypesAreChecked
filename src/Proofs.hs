{-#LANGUAGE EmptyDataDecls #-}

module Proofs where

-- | and introduction
andIntro :: a -> b -> (a, b)
andIntro = (,)

-- | and elimination
andElim1 :: (a, b) -> a
andElim1 = fst

andElim2 :: (a, b) -> b
andElim2 = snd

-- | or introduction
orIntro1 :: a -> Either a b
orIntro1 = Left

orIntro2 :: b -> Either a b
orIntro2 = Right

-- | or elimination
orElim :: (a -> c) -> (b -> c) -> Either a b -> c
orElim = either

-- | implication introduction
implIntro :: a -> a
implIntro = id

-- | implication elimination
implElim :: (a -> b) -> a -> b
implElim = ($)


-- | A proof example
-- Premises:
--     p => q
--     m => p | q
-- Goal:
--     m => q
data P
data Q
data M

-- | p => q
premise1 :: P -> Q
premise1 _ = undefined  -- I can also define it by adding 'Q P' constructor to data Q

-- | m => p | q
premise2 :: M -> Either P Q
premise2 _ = undefined  -- but I don't know how can I properly populate this type, so I guess undefined is fine

-- | m => q
goal :: M -> Q
goal m =
    let step1 = implElim premise2 m :: Either P Q
        step2 = implIntro           :: Q -> Q
    in  orElim premise1 step2 step1 :: Q

