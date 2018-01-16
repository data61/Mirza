{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Lib
import Migrate

main :: IO ()
main = do
  migrate -- make this a command line argument
  startApp connectionStr
-- cmd args -->
    -- run_schema
    -- env (prod/dev --> create DBFunc based on this)
    -- connectionStr
