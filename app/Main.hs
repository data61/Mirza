{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Lib
import Migrate

main :: IO ()
-- main = startApp "supplychain.sqlite"
main = migrate -- make this a command line argument
-- cmd args --> schema, env (prod/dev --> create DBFunc based on this)