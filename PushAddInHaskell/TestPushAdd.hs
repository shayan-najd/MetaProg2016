{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
import PushAddTagless
import Control.Monad.State

test :: Maybe Integer
test = eval [pushAdd| PUSH 1
                      PUSH 2
                      PUSH 0
                      ADD    |]
