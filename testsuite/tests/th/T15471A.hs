{-# LANGUAGE TemplateHaskell #-}
module T15471A where

foo1 x = x

test_foo = [|| foo1 ||]
