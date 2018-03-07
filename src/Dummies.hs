{-# LANGUAGE OverloadedStrings #-}

-- | Sample data types. Serves as a good example of the types defined
-- in GS1Combinators
module Dummies where

import           Data.UUID (nil)
import qualified Model as M
import           Data.GS1.DWhat
import           Data.GS1.DWhy
import           Data.GS1.DWhere
import           Data.GS1.DWhen
import           Data.GS1.EPC
import qualified Data.GS1.Event as Ev
import           Data.Time.LocalTime
import           Data.Time

dummyNewUser :: M.NewUser
dummyNewUser = M.NewUser "000" "fake@gmail.com" "Bob" "Smith" "blah Ltd" "password"

sampleObjectFile :: FilePath
sampleObjectFile = "../GS1Combinators/test/test-xml/ObjectEvent.xml"

dummyUser :: M.User
dummyUser = M.User nil "Sajid" "Anower"

dummyObjectDWhat :: DWhat
dummyObjectDWhat =
  ObjectDWhat
    Add
    [
      dummyLabelEpc,
      IL (SGTIN "0614141" Nothing "107346" "2018")
    ]

-- add function to generate and take dummyLabelEpc

dummyLabelEpc :: LabelEPC
dummyLabelEpc = IL (SGTIN "0614141" Nothing "107346" "2017")

dummyDWhen :: DWhen
dummyDWhen = DWhen
              (read "2013-06-08 14:58:56.591+02:00" :: UTCTime)
              Nothing
              (read "+02:00" :: TimeZone)

dummyDWhere :: DWhere
dummyDWhere = DWhere
                [SGLN "0012345" (LocationReference "11111") (Just "400")]
                -- [ReadPointLocation]
                [SGLN "0012345" (LocationReference "11111") Nothing]
                -- [BizLocation]
                [] []

dummyDWhy :: DWhy
dummyDWhy = DWhy (Just Receiving) (Just InProgress)

dummyEvent :: Ev.Event
dummyEvent =
  Ev.Event
    Ev.ObjectEventT
    Nothing
    dummyObjectDWhat
    dummyDWhen
    dummyDWhy
    dummyDWhere

dummyObjectEvent :: M.ObjectEvent
dummyObjectEvent = M.mkObjectEvent dummyEvent

