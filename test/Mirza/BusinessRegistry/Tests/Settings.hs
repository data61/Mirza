module Mirza.SupplyChain.Tests.Settings where
-- Note: This is probably a bad name for this module, but its not yet clear to me
--       yet what else will go in here. Feel free to name it more appropraitely
--       as it becomes clear what else fits.



import           Data.ByteString.Char8 (ByteString)



-- | Default database connection string used when running tests. Be careful with
-- this construct as it could lead to problems...users not specifying the
-- database and accidentally operating on the wrong database.
testDbConnStr :: ByteString
testDbConnStr = "dbname=testsupplychainserver"
