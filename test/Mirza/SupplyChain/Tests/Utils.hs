{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Tests.Utils where

import           GHC.Generics                          (Generic)

import           Control.Monad.Except
import           Control.Monad.Identity

import           Data.GS1.EPC
import           Data.GS1.Event

import qualified Mirza.BusinessRegistry.Types          as BT
import           Mirza.SupplyChain.Types               as ST

import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)

import           Servant.API.BasicAuth                 (BasicAuthData (..))

import           Mirza.Common.Tests.ServantUtils       (runClient)

import           Text.Email.Validate                   (toByteString)

import           Data.Hashable                         (Hashable (..))
import           Data.HashMap.Lazy                     as H

import           Servant.Client                        (BaseUrl, ClientM)

import           Mirza.BusinessRegistry.Client.Servant as BRClient
import           Mirza.SupplyChain.Client.Servant      as SCSClient

import           Crypto.JOSE                           (Alg (RS256),
                                                        newJWSHeader, signJWS)
import qualified Crypto.JOSE                           as JOSE
import           Crypto.JOSE.Types                     (Base64Octets (..))

import           Mirza.BusinessRegistry.Tests.Generate (genMultipleUsersBR)
import           Mirza.SupplyChain.Tests.Generate

import           Mirza.BusinessRegistry.Tests.Utils    (readJWK)

import           Data.Maybe                            (fromJust)

import           Data.List.NonEmpty                    (NonEmpty (..))


-- =============================================================================
-- Utility Data structures/Type aliases
-- =============================================================================

type EntityName = T.Text
type BusinessName = T.Text

data EachEvent = EachEvent {
    eachEventEntities :: [Entity]
  , eachEventEvent    :: Event
  }
  deriving (Eq, Show, Generic)

data Entity = Entity {
    entitiName          :: EntityName
  , entityCompanyPrefix :: GS1CompanyPrefix
  , entityBizName       :: BusinessName
  , entityLocList       :: [LocationEPC]
  , entityKeyPairPaths  :: KeyPairPaths
  }
  deriving (Eq, Show, Generic)
instance Hashable Entity where
  hashWithSalt salt entity = hashWithSalt salt (unGS1CompanyPrefix $ entityCompanyPrefix entity)

--                              PrivateKey PublicKey
data KeyPairPaths = KeyPairPaths FilePath FilePath
  deriving (Eq, Show, Generic)

privateKeyPath :: String -> FilePath
privateKeyPath entityName = "./test/Mirza/SupplyChain/TestData/testKeys/goodJWKs/" <> entityName <> "_rsa.json"

publicKeyPath :: String -> FilePath
publicKeyPath entityName = "./test/Mirza/SupplyChain/TestData/testKeys/goodJWKs/" <> entityName <> "_rsa_pub.json"

type AuthHash = H.HashMap Entity (UserId, BasicAuthData, BRKeyId)
type LocationMap = H.HashMap LocationEPC BT.NewLocation


-- =============================================================================
-- Insertion and Signature utils
-- =============================================================================

insertAndAuth :: BaseUrl
              -> BaseUrl
              -> BasicAuthData
              -> LocationMap
              -> AuthHash
              -> [Entity]
              -> IO AuthHash
insertAndAuth _ _ _ _          ht [] = pure ht
insertAndAuth scsUrl brUrl auth locMap ht (entity:entities) = do
  let httpSCS = runClient scsUrl
      httpBR = runClient brUrl
      (Entity name companyPrefix bizName locations (KeyPairPaths _ pubKeyPath)) = entity
      [newUserSCS] = genMultipleUsersSCS "citrusTest" 1 [name] [companyPrefix]
      [newUserBR] = genMultipleUsersBR "citrusTest" 1 [name] [companyPrefix]
      newBiz = BT.NewBusiness companyPrefix bizName
  Just pubKey <- liftIO $ readJWK pubKeyPath
  Right insertedUserIdSCS <- httpSCS $ SCSClient.addUser newUserSCS
  _insertedUserIdBR <- httpBR $ BRClient.addUser auth newUserBR
  _insertedPrefix <- httpBR $ BRClient.addBusiness auth newBiz
  Right brKeyId <- httpBR $ BRClient.addPublicKey auth pubKey Nothing
  let basicAuthDataSCS =
        BasicAuthData
          (toByteString . ST.newUserEmailAddress $ newUserSCS)
          (encodeUtf8   . ST.newUserPassword     $ newUserSCS)

  let newLocs = flip H.lookup locMap <$> locations
  sequence_ $ maybeInsertLocation <$> newLocs
  let updatedHt = H.insert entity (insertedUserIdSCS, basicAuthDataSCS, brKeyId) ht
  insertAndAuth scsUrl brUrl auth locMap updatedHt entities
  where
    maybeInsertLocation Nothing    = pure ()
    maybeInsertLocation (Just loc) = void $ runClient brUrl $ BRClient.addLocation auth loc

insertEachEvent :: AuthHash -> EachEvent ->  ClientM ()
insertEachEvent _ (EachEvent [] _) = pure ()
insertEachEvent ht (EachEvent (initialEntity: entities) ev) = do
  let Just (entityUserId, auth, _) = H.lookup initialEntity ht
  (insertedEventInfo, _eventId) <- case _etype ev of
          AggregationEventT -> SCSClient.insertAggEvent auth (fromJust $ mkAggEvent ev)
          ObjectEventT -> SCSClient.insertObjectEvent auth (fromJust $ mkObjectEvent ev)
          TransactionEventT -> SCSClient.insertTransactEvent auth (fromJust $ mkTransactEvent ev (entityUserId :| []))
          TransformationEventT -> SCSClient.insertTransfEvent auth (fromJust $ mkTransfEvent ev)

  sequence_ $ clientSignEvent ht insertedEventInfo <$> entities


clientSignEvent :: AuthHash -> EventInfo -> Entity -> ClientM EventInfo
clientSignEvent ht evInfo entity = do
  let Just (_, auth, _) = H.lookup entity ht
      (EventInfo event _ _ (Base64Octets toSign) _) = evInfo
      eventId = fromJust $ _eid event
      (Entity _ _ _ _ (KeyPairPaths privKeyPath pubKeyPath)) = entity
  Just privKey <- liftIO $ readJWK privKeyPath
  Just pubKey <- liftIO $ readJWK pubKeyPath
  keyId <- BRClient.addPublicKey auth pubKey Nothing

  Right mySig <- liftIO $ runExceptT @JOSE.Error (
          signJWS toSign (Identity (newJWSHeader ((), RS256), privKey))
          )
  let signedEvent = SignedEvent eventId keyId mySig
  eventSign auth signedEvent

