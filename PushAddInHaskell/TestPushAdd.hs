{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
import PushAdd
import Control.Monad.State

test = evalState [sb| PUSH 1
                      PUSH 2
                      PUSH 0
                      ADD    |] []
