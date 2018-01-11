{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Lib
import Migrate

main :: IO ()
-- main = startApp "supplychain.sqlite"
main = migrate
