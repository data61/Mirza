{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans            #-}


module Mirza.Common.GS1Orphans where


import           Data.GS1.EventId

import           Servant          (ToHttpApiData)


deriving instance ToHttpApiData EventId
