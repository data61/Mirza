module Main where

import           Mirza.Trails.Tests.Client

import           Test.Tasty                hiding (withResource)
import           Test.Tasty.Runners        (NumThreads (..))

import           Control.Exception         (bracket)
import           Control.Monad.Except      (liftIO, runExceptT)

main :: IO ()
main = do
  either (error . show) pure =<< (liftIO $ runExceptT $ makeDatabase testDbNameTrails)

  clientTests <- clientSpec

  defaultMain $ localOption (NumThreads 1) $ testGroup "tests"
    [ clientTests
    ]
