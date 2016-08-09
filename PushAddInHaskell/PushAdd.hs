{-# LANGUAGE QuasiQuotes,TemplateHaskell #-}
module PushAdd where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Control.Monad.State

data Op = PUSH Integer | ADD
        deriving (Read,Show)

push :: Integer -> State [Integer] ()
push i = modify (i :)

add :: State [Integer] (Maybe Integer)
add = do s <- get
         return (if length s < 2
                 then Nothing
                 else Just (s !! 0 +  s !! 1))

sbExp :: String -> Q Exp
sbExp s = return
          (DoE [ NoBindS (case read l of
                            ADD     -> VarE 'add
                            PUSH  i -> AppE (VarE 'push) (LitE (IntegerL i)))
               | l  <- lines s])

sb :: QuasiQuoter
sb =  QuasiQuoter {quoteExp  = sbExp,
                   quotePat  = undefined,
                   quoteType = undefined,
                   quoteDec  = undefined}
