-- | This module contains the
-- Database.Beam.Postgres.Syntax.DataType definitions
-- At the moment, if Database.Beam.Postgres.Syntax is a hidden module
-- So it is not possible to implement the types yet
module MigrateUtils where

import qualified Data.GS1.EPC                  as EPC
import qualified Data.GS1.Event                as Ev
import           Database.Beam.Migrate         (DataType (..))
import           Database.Beam.Postgres.Syntax (PgDataTypeSyntax, pgTextType)

-- | Shorthand for using postgres text type
textType :: DataType PgDataTypeSyntax a
textType = DataType pgTextType

eventType :: DataType PgDataTypeSyntax Ev.EventType
eventType = textType

actionType :: DataType PgDataTypeSyntax EPC.Action
actionType = textType
