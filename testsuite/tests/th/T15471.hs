{-# LANGUAGE TemplateHaskell #-}
module T15471 where

import T15471A


qux = $$(test_foo)

main = print (qux 5)
