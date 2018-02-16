module Strict.Demo where

import Strict.Strict

import Control.Monad.Identity
import Debug.Trace

demo :: IO ()
demo = do
    print $ runIdentity demoIdentity
    print $ runStrict demoStrict

  where
    demoIdentity :: Identity Int
    demoIdentity = do
        a <- trace "r11I"  return $ trace "11I" 11
        b <- trace "r12I"  return $ trace "12I" 12
        c <- trace "r13I"  return $ trace "13I" 13
        return $ (b + c) * a

    demoStrict :: Strict Int
    demoStrict = do
        a <- trace "r11S"  return $ trace "11S" 11
        b <- trace "r12S"  return $ trace "12S" 12
        c <- trace "r13S"  return $ trace "13S" 13
        return $ (b + c) * a
