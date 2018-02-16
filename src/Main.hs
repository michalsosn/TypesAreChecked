{-#LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import qualified Arith
import qualified Hungry
import qualified Security
import qualified Total
import qualified Expanding
import qualified Proven.Demo as Proven
import qualified Reccer
import qualified Strict.Demo as Strict

class Chuj a where
    chuj :: Int -> [a]

class Dupa a where
    dupa :: a

data KamieniKupa = KamieniKupa
    deriving Show

instance Dupa a => Chuj a where
    chuj n = replicate n dupa

instance Chuj a => Dupa a where
    dupa = head $ chuj 1

kamieniKupuj :: (KamieniKupa, KamieniKupa)
kamieniKupuj = let [a:_:b:[]] = chuj 3 in (a, b)

main :: IO ()
main = do
--    print (dupa :: KamieniKupa)
--    print (chuj 3 :: [KamieniKupa])
    --print kamieniKupuj
    --Arith.main
--    Hungry.demo
--    Security.demo
--    Total.demo
--    Expanding.demo
--    Reccer.demo
    Proven.demo
--    Strict.demo
