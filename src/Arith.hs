module Arith (main) where

import Control.Monad.Error
import Control.Monad.State
import Data.Array
import System.Random

data Info = Info
    { file :: String
    , line :: Int
    }

instance Show Info where
    show (Info file line) = file ++ "@" ++ show line

dummyInfo :: Info
dummyInfo = Info "" 0

data Term =
    TmTrue Info
  | TmFalse Info
  | TmIf Info Term Term Term
  | TmZero Info
  | TmSucc Info Term
  | TmPred Info Term
  | TmIsZero Info Term
        deriving Show

data EvalError =
    NoRuleApplies
  | OtherError String

instance Show EvalError where
    show NoRuleApplies    = "No rule applies"
    show (OtherError msg) = msg

type EvalMonad = Either EvalError

isNumericVal :: Term -> Bool
isNumericVal (TmZero _)    = True
isNumericVal (TmSucc _ t1) = isNumericVal t1
isNumericVal _             = False

isVal :: Term -> Bool
isVal (TmTrue _)  = True
isVal (TmFalse _) = True
isVal t           = isNumericVal t

eval1 :: Term -> EvalMonad Term
eval1 (TmIf _ (TmTrue _) t2 _)  = return t2
eval1 (TmIf _ (TmFalse _) _ t3) = return t3
eval1 (TmIf fi t1 t2 t3)        = do
    t1' <- eval1 t1
    return $ TmIf fi t1' t2 t3
eval1 (TmSucc fi t1)            = TmSucc fi `fmap` eval1 t1
eval1 (TmPred _ (TmZero _))     = return $ TmZero dummyInfo
eval1 (TmPred _ (TmSucc _ nv1))
    | isNumericVal nv1          = return nv1
eval1 (TmPred fi t1)            = TmPred fi `fmap` eval1 t1
eval1 (TmIsZero _ (TmZero _))   = return $ TmTrue dummyInfo
eval1 (TmIsZero _ (TmSucc _ nv1))
    | isNumericVal nv1          = return $ TmFalse dummyInfo
eval1 (TmIsZero fi t1)          = TmIsZero fi `fmap` eval1 t1
eval1 _                         = throwError NoRuleApplies

evalSmall :: Term -> EvalMonad Term
evalSmall t = (eval1 t >>= evalSmall) `catchError` finishEval
    where
        finishEval NoRuleApplies = Right t
        finishEval e             = Left e

evalBig :: Term -> EvalMonad Term
evalBig v | isVal v = return v
evalBig (TmIf fi t1 t2 t3) = evalBig t1 >>= handleIf
    where
        handleIf (TmTrue _)  = evalBig t2
        handleIf (TmFalse _) = evalBig t3
        handleIf _           = throwError NoRuleApplies
evalBig (TmSucc fi t1)     = TmSucc fi `fmap` evalBig t1
evalBig (TmPred fi t1)     = evalBig t1 >>= handlePred
    where
        handlePred (TmZero _)   = return $ TmZero dummyInfo
        handlePred (TmSucc _ nv1)
            | isNumericVal nv1  = return nv1
        handlePred _            = throwError NoRuleApplies
evalBig (TmIsZero fi t1)   = evalBig t1 >>= handleIsZero
    where
        handleIsZero (TmZero _)   = return $ TmTrue dummyInfo
        handleIsZero (TmSucc _ nv1)
            | isNumericVal nv1    = return $ TmFalse dummyInfo
        handleIsZero _            = throwError NoRuleApplies

randomTake :: (Random i, Ix i, RandomGen g) => Array i a -> g -> (a, g)
randomTake arr g = let (i, g') = randomR (bounds arr) g in (arr ! i, g')

data MakerState g = MakerState { mkrGen :: g, mkrLine :: Int }

--makeTree :: RandomGen g => String -> Int -> State g Term
--makeTree name depth = return (TmZero dummyInfo)
--    where
--        leaves = listArray (0, 2) [TmTrue, TmFalse, TmZero]
--        inner1 = listArray (0, 3) [TmSucc, TmPred, TmIsZero]
--        inner3 = listArray (0, 0) [TmIf]
--
--        makeLeaf :: RandomGen g => State (MakerState g) Term
--        makeLeaf = do
--            line <- gets mkrLine

main :: IO ()
main = do
    let es = [ TmIsZero dummyInfo (TmSucc dummyInfo (TmZero dummyInfo))
             , TmIsZero dummyInfo (TmSucc dummyInfo (TmTrue dummyInfo))
             ]
    forM_ es $ \e -> do
        let e1 = evalSmall e
            e2 = evalBig e
        putStrLn $ either show show e1
        putStrLn $ either show show e2

