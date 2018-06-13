{-# LANGUAGE ConstraintKinds #-}

module Mirza.SupplyChain.Handlers.Common where



import           Mirza.SupplyChain.Types



type SCSApp context err =
  ( AsServiceError err
  , DBConstraint context err
  )
