{-# LANGUAGE ConstraintKinds #-}

module Mirza.BusinessRegistry.Handlers.Common where



import           Mirza.BusinessRegistry.Types (AsBRError)
import           Mirza.Common.Types           (DBConstraint)



type BRApp context err =
  ( AsBRError err -- TODO: Remove this
  , DBConstraint context err
  )
