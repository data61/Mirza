{-# LANGUAGE ConstraintKinds #-}

module Mirza.BusinessRegistry.Handlers.Common where



import           Mirza.BusinessRegistry.Types
import           Mirza.Common.Types           (DBConstraint)
import           Mirza.SupplyChain.Types      (AsServiceError)



type BRApp context err =
  ( AsServiceError err
  , DBConstraint context err
  )
