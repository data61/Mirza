{-# LANGUAGE ConstraintKinds #-}

module Mirza.BusinessRegistry.Handlers.Common where



import           Mirza.BusinessRegistry.Types (AsBusinessRegistryError)
import           Mirza.Common.Types           (DBConstraint)



type BRApp context err =
  ( AsBusinessRegistryError err -- TODO: Remove this
  , DBConstraint context err
  )