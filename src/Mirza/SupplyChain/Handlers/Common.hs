{-# LANGUAGE ConstraintKinds #-}

module Mirza.SupplyChain.Handlers.Common where

import           Mirza.SupplyChain.Types

type SCSApp context err =
  ( AsServiceError err
  , AsSqlError err
  , HasEnvType context
  , HasConnPool context
  )
