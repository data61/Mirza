module Mirza.BusinessRegistry.Handlers.Health
  ( health
  ) where


import           Mirza.BusinessRegistry.Types             as BT


-- | Currently the health check always returns success and is basically just a
-- confirmation that the process is still alive and hasn't died or blocked.
health :: AppM context err HealthResponse
health = pure HealthResponse
