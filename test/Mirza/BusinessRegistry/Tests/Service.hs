{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.BusinessRegistry.Tests.Service
  ( testServiceQueries
  ) where

import           Mirza.BusinessRegistry.Tests.Dummies

import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types                 as BT
import           Mirza.SupplyChain.Handlers.Business
import           Mirza.SupplyChain.Handlers.EventRegistration
import           Mirza.SupplyChain.Handlers.Users
import qualified Mirza.SupplyChain.StorageBeam                as SB
import           Mirza.SupplyChain.Types                      as ST

import           Data.Either                                  (isLeft)
import           Data.Maybe                                   (fromJust,
                                                               isNothing)

import qualified Data.Text                                    as T

import           Data.Time.Clock                              (addUTCTime,
                                                               getCurrentTime)
import           Data.Time.LocalTime                          (LocalTime, utc,
                                                               utcToLocalTime)
import           GHC.Stack                                    (HasCallStack)
import           Test.Hspec

timeStampIO :: MonadIO m => m LocalTime
timeStampIO = liftIO $ (utcToLocalTime utc) <$> getCurrentTime

rsaPubKey :: IO BT.PEM_RSAPubKey
rsaPubKey = BT.PEM_RSAPubKey . T.pack <$> Prelude.readFile "./test/Mirza/BusinessRegistry/Tests/testKeys/goodKeys/test.pub"

testAppM :: context -> AppM context BusinessRegistryError a -> IO a
testAppM brContext act = runAppM brContext act >>= \case
    Left err -> fail (show err)
    Right a -> pure a

testServiceQueries :: HasCallStack => SpecWith BT.BRContext
testServiceQueries = do

  describe "addPublicKey tests" $
    it "addPublicKey test 1" $ \brContext -> do
      pubKey <- rsaPubKey
      tStart <- timeStampIO
      res <- testAppM brContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        let (BT.PEM_RSAPubKey keyStr) = pubKey
        keyId <- addPublicKey user pubKey Nothing
        tEnd <- timeStampIO
        insertedKey <- getPublicKey keyId
        storageKey <- runDb $ getKeyById keyId
        pure (storageKey, keyStr, keyId, uid, tEnd, insertedKey)
      case res of
        (Nothing, _, _, _, _, _) -> fail "Received Nothing for key"
        (Just key, keyStr, (KeyID keyId), (UserID uid), tEnd, insertedKey) -> do
          key `shouldSatisfy`
            (\k ->
              (SB.pem_str k) == keyStr &&
              (SB.key_id k) == keyId &&
              (SB.key_user_id k) == (SB.UserId uid) &&
              (SB.creation_time k) > tStart &&
              (SB.creation_time k) < tEnd &&
              isNothing (SB.revocation_time k)
            )
          insertedKey `shouldBe` pubKey
  describe "getPublicKeyInfo tests" $
    it "getPublicKeyInfo test 1" $ \brContext -> do
      tStart <- liftIO getCurrentTime
      pubKey <- rsaPubKey
      (keyInfo, uid, tEnd) <- testAppM brContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- addPublicKey user pubKey Nothing
        keyInfo <- getPublicKeyInfo keyId
        tEnd <- liftIO getCurrentTime
        pure (keyInfo, uid, tEnd)
      keyInfo `shouldSatisfy`
        (\ki ->
          (keyInfoUserId ki == uid) &&
          ((unCreationTime . creationTime $ ki) > tStart &&
           (unCreationTime . creationTime $ ki) < tEnd) &&
          isNothing (revocationTime ki)
        )

  describe "revokePublicKey tests" $ do
    it "Revoke public key with permissions" $ \brContext -> do
      pubKey <- rsaPubKey
      myKeyState <- testAppM brContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- addPublicKey user pubKey Nothing
        _timeKeyRevoked <- revokePublicKey user keyId
        keyInfo <- getPublicKeyInfo keyId
        pure (keyState keyInfo)
      myKeyState `shouldBe` Revoked

    it "Revoke public key without permissions" $ \brContext -> do
      pubKey <- rsaPubKey
      r <- runAppM @_ @ServiceError brContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- addPublicKey user pubKey Nothing

        -- making a fake user
        hackerUid <- newUser $ makeDummyNewUser (EmailAddress "l33t@hacker.com")
        storageHacker <- runDb $ getUserById hackerUid
        let hacker = userTableToModel . fromJust $ storageHacker

        revokePublicKey hacker keyId
      r `shouldSatisfy` isLeft
      -- r `shouldBe` Left BT.UnauthorisedKeyAccess

    it "Already expired AND revoked pub key" $ \brContext -> do
      nowish <- getCurrentTime
      let hundredMinutes = 100 * 60
          someTimeAgo = addUTCTime (-hundredMinutes) nowish
      pubKey <- rsaPubKey
      myKeyState <- testAppM brContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- addPublicKey user pubKey (Just . ExpirationTime $ someTimeAgo )
        _timeKeyRevoked <- revokePublicKey user keyId
        keyInfo <- getPublicKeyInfo keyId
        pure (keyState keyInfo)
      myKeyState `shouldBe` Revoked

    it "Expired but NOT revoked pub key" $ \brContext -> do
      nowish <- getCurrentTime
      let hundredMinutes = 100 * 60
          someTimeAgo = addUTCTime (-hundredMinutes) nowish
      pubKey <- rsaPubKey
      myKeyState <- testAppM brContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- addPublicKey user pubKey (Just . ExpirationTime $ someTimeAgo)
        keyInfo <- getPublicKeyInfo keyId
        pure (keyState keyInfo)
      myKeyState `shouldBe` Expired

    it "Expiry date in the future" $ \brContext -> do
      nowish <- getCurrentTime
      let hundredMinutes = 100 * 60
          someTimeLater = addUTCTime hundredMinutes nowish
      pubKey <- rsaPubKey
      myKeyState <- testAppM brContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- addPublicKey user pubKey (Just . ExpirationTime $ someTimeLater)
        keyInfo <- getPublicKeyInfo keyId
        pure (keyState keyInfo)
      myKeyState `shouldBe` InEffect

  describe "Business" $ do
    it "List Business empty" $ \brContext -> do
      bizList <- testAppM brContext listBusinesses
        -- myBizList <-
        -- pure listBusinesses
      bizList `shouldBe` []
