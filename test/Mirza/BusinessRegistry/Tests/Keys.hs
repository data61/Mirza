{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Mirza.BusinessRegistry.Tests.Keys
  ( testKeyQueries
  ) where

import           Mirza.BusinessRegistry.Database.Schema   as BSchema

import           Mirza.BusinessRegistry.Handlers.Business
import           Mirza.BusinessRegistry.Handlers.Keys     as BKey
import           Mirza.BusinessRegistry.Handlers.Users
import           Mirza.BusinessRegistry.Tests.Dummies
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Time                        (CreationTime (..),
                                                           ExpirationTime (..))

import           Mirza.BusinessRegistry.Tests.Utils

import           Data.Maybe                               (fromJust, isNothing)

import           Data.Time.Clock                          (addUTCTime,
                                                           getCurrentTime)
import           Data.Time.LocalTime                      (LocalTime, utc,
                                                           utcToLocalTime)
import           GHC.Stack                                (HasCallStack)
import           Test.Hspec

import           Control.Concurrent                     (threadDelay)

timeStampIO :: MonadIO m => m LocalTime
timeStampIO = liftIO $ (utcToLocalTime utc) <$> getCurrentTime

testAppM :: context
         -> AppM context BusinessRegistryError a
         -> IO a
testAppM brContext act = runAppM brContext act >>= \case
    Left err -> fail (show err)
    Right a -> pure a


testKeyQueries :: (HasCallStack) => SpecWith BT.BRContext
testKeyQueries = do

  describe "addPublicKey tests" $
    it "addPublicKey test 1" $ \brContext -> do
      pubKey <- goodRsaPublicKey
      tStart <- timeStampIO
      res <- testAppM brContext $ do
        user <- insertDummies
        let uid = authUserId user
        let (BT.PEM_RSAPubKey keyStr) = pubKey
        keyId <- addPublicKey user pubKey Nothing
        tEnd <- timeStampIO
        insertedKey <- getPublicKey keyId
        storageKey <- runDb $ BKey.getKeyById keyId
        pure (storageKey, keyStr, keyId, uid, tEnd, insertedKey)
      case res of
        (Nothing, _, _, _, _, _) -> fail "Received Nothing for key"
        (Just key, keyStr, (BRKeyId keyId), (BT.UserId uid), tEnd, insertedKey) -> do
          key `shouldSatisfy`
            (\k ->
              (BSchema.pem_str k) == keyStr &&
              (BSchema.key_id k) == keyId &&
              (BSchema.key_user_id k) == (BSchema.UserId uid) &&
              (BSchema.creation_time k) > tStart &&
              (BSchema.creation_time k) < tEnd &&
              isNothing (BSchema.revocation_time k)
            )
          insertedKey `shouldBe` pubKey
  describe "getPublicKeyInfo tests" $
    it "getPublicKeyInfo test 1" $ \brContext -> do
      tStart <- liftIO getCurrentTime
      pubKey <- goodRsaPublicKey
      (keyInfo, uid, tEnd) <- testAppM brContext $ do
        user <- insertDummies
        let uid = authUserId user
        keyId <- addPublicKey user pubKey Nothing
        keyInfo <- getPublicKeyInfo keyId
        tEnd <- liftIO getCurrentTime
        pure (keyInfo, uid, tEnd)
      keyInfo `shouldSatisfy`
        (\ki ->
          (keyInfoUserId ki == uid) &&
          ((keyInfoCreationTime ki) > (CreationTime tStart) &&
           (keyInfoCreationTime ki) < (CreationTime tEnd)) &&
          isNothing (keyInfoRevocationTime ki)
        )

  describe "revokePublicKey tests" $ do
    it "Revoke public key with permissions" $ \brContext -> do
      pubKey <- goodRsaPublicKey
      myKeyState <- testAppM brContext $ do
        user <- insertDummies
        keyId <- addPublicKey user pubKey Nothing
        _timeKeyRevoked <- revokePublicKey user keyId
        keyInfo <- getPublicKeyInfo keyId
        pure (keyInfoState keyInfo)
      myKeyState `shouldBe` Revoked

{-- XXX - FIXME!!! Need to catch UnAuthorisedKeyAccess error
    it "Revoke public key without permissions" $ \brContext -> do
      pubKey <- goodRsaPublicKey
      r <- testAppM brContext $ do
        uid <- addUser dummyNewUser {newUserCompany=businessPfx}
        tableUser <- runDb $ getUserByIdQuery uid
        let user = tableToAuthUser . fromJust $ tableUser
        keyId <- addPublicKey user pubKey Nothing

        -- making a fake user
        hackerUid <- addUser $ makeDummyNewUser (EmailAddress "l33t@hacker.com")
        storageHacker <- runDb $ getUserByIdQuery hackerUid
        let hacker = tableToAuthUser . fromJust $ storageHacker

        revokePublicKey hacker keyId
      r `shouldSatisfy` isLeft
      -- r `shouldBe` Left BT.UnauthorisedKeyAccess
--}

    it "Expired but NOT revoked pub key" $ \brContext -> do
      nowish <- getCurrentTime
      let smallDelay = 1
          nearExpiry = addUTCTime (fromInteger smallDelay) nowish
      pubKey <- goodRsaPublicKey
      keyId <- testAppM brContext $ do
        user <- insertDummies
        keyId <- addPublicKey user pubKey (Just . ExpirationTime $ nearExpiry)
        pure keyId
      threadDelay $ fromIntegral $ secondsToMicroseconds smallDelay
      myKeyState <- testAppM brContext $ do
        keyInfo <- getPublicKeyInfo keyId
        pure (keyInfoState keyInfo)
      myKeyState `shouldBe` Expired

    it "Expiry date in the future" $ \brContext -> do
      nowish <- getCurrentTime
      let hundredMinutes = 100 * 60
          someTimeLater = addUTCTime hundredMinutes nowish
      pubKey <- goodRsaPublicKey
      myKeyState <- testAppM brContext $ do
        user <- insertDummies
        keyId <- addPublicKey user pubKey (Just . ExpirationTime $ someTimeLater)
        keyInfo <- getPublicKeyInfo keyId
        pure (keyInfoState keyInfo)
      myKeyState `shouldBe` InEffect

  describe "Business" $ do
    it "List Business empty" $ \brContext -> do
      bizList <- testAppM brContext listBusinesses
        -- myBizList <-
        -- pure listBusinesses
      bizList `shouldBe` []


-- *****************************************************************************
-- Test Utility Functions
-- *****************************************************************************

-- | Adds the dummy business and user and returns the user id and auth user.
insertDummies :: AppM BRContext BusinessRegistryError AuthUser
insertDummies = do
  businessPfx <- addBusiness dummyBusiness
  uid <- addUser dummyNewUser {newUserCompany=businessPfx}
  tableUser <- runDb $ getUserByIdQuery uid
  return (tableToAuthUser . fromJust $ tableUser)
