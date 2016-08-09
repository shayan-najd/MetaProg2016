{-# LANGUAGE QuasiQuotes,TemplateHaskell, MultiParamTypeClasses,
             FlexibleInstances, GADTs #-}
module PushAddTagless where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Control.Monad.State

data Op = PUSH Integer | ADD
        deriving (Read,Show)

class Monad m => OpLike m where
 push :: Integer -> m ()
 add  :: m (Maybe Integer)


sbExp :: String -> Q Exp
sbExp s = return
          (DoE [ NoBindS (case read l of
                            ADD     -> VarE 'add
                            PUSH  i -> AppE (VarE 'push) (LitE (IntegerL i)))
               | l  <- lines s])

pushAdd :: QuasiQuoter
pushAdd =  QuasiQuoter {quoteExp  = sbExp,
                        quotePat  = undefined,
                        quoteType = undefined,
                        quoteDec  = undefined}

instance OpLike (State [Integer]) where
  push i = modify (i :)
  add    = do s <- get
              return (if length s < 2
                      then Nothing
                      else Just (s !! 0 +  s !! 1))

eval :: State [Integer] a -> a
eval m = evalState m ([] :: [Integer])
