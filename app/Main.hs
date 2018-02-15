{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Lib
import           Migrate
import           Data.ByteString (ByteString)
import           Migrate (defConnectionStr)
import           Options.Applicative
import           Data.Semigroup ((<>))
import           AppConfig (EnvType(..))
import           Data.GS1.Parser.Parser
import           Data.Aeson.Encode.Pretty
import           Data.GS1.Event
import           Data.Either
import           Text.XML
import           Text.XML.Cursor
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           System.Environment
import           Model as M
import           Data.UUID (nil)
import           BeamQueries as BQ
import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhy
import           Data.GS1.DWhere
import           Data.GS1.EPC

data ServerOptions = ServerOptions
  { env           :: EnvType
  , initDB        :: Bool
--  , clearDB       :: Bool
  , connectionStr :: ByteString
  , port          :: Int
  , uiFlavour     :: UIFlavour
  }

serverOptions :: Parser ServerOptions
serverOptions = ServerOptions
      <$> option auto
          ( long "env"
         <> short 'e'
         <> help "Environment, Dev | Prod"
         <> showDefault
         <> value Dev )
      <*> switch
          ( long "init-db"
         <> short 'i'
         <> help "Put empty tables into a fresh database" )
    --   <*> switch
    --       ( long "clear-db"
    --      <> short 'e'
    --      <> help "Erase the database - DROP ALL TABLES" )
      <*> option auto
          ( long "conn"
         <> short 'c'
         <> help "database connection string"
         <> showDefault
         <> value defConnectionStr)
       <*> option auto
          ( long "port"
         <> help "Port to run database on"
         <> showDefault
         <> value 8000)
       <*> option auto
          ( long "uiFlavour"
         <> help "Use jensoleg or Original UI Flavour for the Swagger API"
         <> showDefault
         <> value Original)


main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (serverOptions <**> helper)
      (fullDesc
      <> progDesc "Run a supply chain server"
      <> header "SupplyChainServer - A server for capturing GS1 events and recording them on a blockchain")


-- Sara's
-- runProgram :: ServerOptions -> IO ()
-- runProgram (ServerOptions isDebug False _connStr portNum flavour) =
--     startApp connStr isDebug (fromIntegral portNum) flavour
-- runProgram (ServerOptions _ _ True connStr portNum flavour) =
--     startApp connStr isDebug (fromIntegral portNum) flavour
-- runProgram _ = migrate defConnectionStr
runProgram :: ServerOptions -> IO ()
runProgram (ServerOptions envT False connStr portNum flavour) =
    startApp connStr envT (fromIntegral portNum) flavour
runProgram _ = migrate defConnectionStr
-- runProgram _ = runMonkeyPatch

-- fileToParse :: FilePath
-- fileToParse = "../GS1Combinators/test/test-xml/ObjectEvent.xml"

-- dummyUser :: M.User
-- dummyUser = M.User nil "Sajid" "Anower"

-- runMonkeyPatch = do
--   doc <- Text.XML.readFile def fileToParse
--   let mainCursor = fromDocument doc
--   -- scope for optimization: only call parseEventByType on existent EventTypes
--       allParsedEvents =
--         filter (not . null) $ concat $
--         parseEventByType mainCursor <$> allEventTypes
--       (Right objEvent) = head allParsedEvents
--       newObj = eventToNewObject objEvent
--   eventId <- BQ.eventCreateObject dummyUser newObj
--   print eventId
--   print objEvent

--   -- mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (rights allParsedEvents)


-- eventToNewObject :: Event -> M.NewObject
-- eventToNewObject
--   (Event evType mEid
--     (ObjectDWhat act epcs)
--     (DWhen tStamp _ tZone)
--     dwhy
--     dwhere) = do
--   let
--       labelEpc = head epcs
--       rPoint = head . _readPoint $ dwhere
--       eLocation = M.EventLocation rPoint rPoint (SDOwningParty, rPoint) (SDOwningParty, rPoint)
--       newObj = M.NewObject labelEpc tStamp tZone eLocation mEid
--   newObj  
      -- tStamp = _eventTime dwhen
      -- tZone = _timeZone dwhen


-- Event
--   {_type = ObjectEventT, _eid = Just (EventID {getEventId = b1080840-e9cc-11e6-bf0e-fe55135034f3}),
--   _what = ObjectDWhat {_objAction = Observe, _objEpcList = [IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = "0614141", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = "107346", _sgtinSerialNum = "2017"}},IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = "0614141", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = "107346", _sgtinSerialNum = "2018"}}]},
--   _when = DWhen {_eventTime = 2005-04-04 02:33:31.116 UTC, _recordTime = Just 2005-04-04 02:33:31.116 UTC, _timeZone = -0600},
--   _why = DWhy {_DWhyBizStep = Just Shipping, _DWhyDisposition = Just InTransit},
--   _where = DWhere {_readPoint = [SGLN {_sglnCompanyPrefix = "0614141", _locationRef = LocationReference {_locationRefVal = "07346"}, _sglnExt = Just "1234"}], _bizLocation = [], _srcType = [], _destType = []})


