{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Mirza.OrgRegistry.Tests.Keys
  ( testKeyQueries
  ) where

import           Mirza.OrgRegistry.Database.Schema as BSchema

import           Mirza.Common.Time                 (CreationTime (..),
                                                    ExpirationTime (..),
                                                    RevocationTime (..))
import           Mirza.Common.Utils                (fromPgJSON)
import           Mirza.OrgRegistry.Auth            (tableUserToAuthUser)
import           Mirza.OrgRegistry.Handlers.Keys   as BKey
import           Mirza.OrgRegistry.Handlers.Org
import           Mirza.OrgRegistry.Handlers.Users
import           Mirza.OrgRegistry.Tests.Dummies
import           Mirza.OrgRegistry.Types           as ORT

import           Mirza.OrgRegistry.Tests.Utils

import           Data.Maybe                        (fromJust, isNothing)

import           Data.Time.Clock                   (NominalDiffTime, UTCTime,
                                                    addUTCTime, getCurrentTime)
import           Data.Time.LocalTime               (LocalTime, utc,
                                                    utcToLocalTime)
import           GHC.Stack                         (HasCallStack)
import           Test.Hspec

import           Control.Concurrent                (threadDelay)


timeStampIO :: MonadIO m => m LocalTime
timeStampIO = liftIO $ (utcToLocalTime utc) <$> getCurrentTime

testAppM :: context -> AppM context ORError a -> IO a
testAppM orContext act = runAppM orContext act >>= \case
    Left err -> fail (show err)
    Right a -> pure a


testKeyQueries :: HasCallStack => SpecWith ORT.ORContextMinimal
testKeyQueries = do

  describe "addPublicKey tests" $
    it "addPublicKey test 1" $ \orContext -> do
      Just pubKey <- goodRsaPublicKey
      tStart <- timeStampIO
      res <- testAppM orContext $ do
        user <- insertDummies
        let uid = authUserId user
        keyId <- addPublicKey user pubKey Nothing
        tEnd <- timeStampIO
        insertedKey <- getPublicKey keyId
        storageKey <- runDb $ BKey.getKeyById keyId
        pure (storageKey, keyId, uid, tEnd, insertedKey)
      case res of
        (Nothing, _, _, _, _) -> fail "Received Nothing for key"
        (Just key, (ORKeyId keyId), oAuthSub, tEnd, insertedKey) -> do
          key `shouldSatisfy`
            (\k ->
              (fromPgJSON $ BSchema.key_jwk k) == pubKey &&
              (BSchema.key_id k) == keyId &&
              (BSchema.key_user_id k) == (BSchema.UserPrimaryKey oAuthSub) &&
              (BSchema.creation_time k) > tStart &&
              (BSchema.creation_time k) < tEnd &&
              isNothing (BSchema.revocation_time k)
            )
          insertedKey `shouldBe` pubKey
  describe "getPublicKeyInfo tests" $
    it "getPublicKeyInfo test 1" $ \orContext -> do
      tStart <- liftIO getCurrentTime
      Just pubKey <- goodRsaPublicKey
      (keyInfo, uid, tEnd) <- testAppM orContext $ do
        user <- insertDummies
        let uid = authUserId user
        keyId <- addPublicKey user pubKey Nothing
        keyInfo <- getPublicKeyInfo keyId
        tEnd <- liftIO getCurrentTime
        pure (keyInfo, uid, tEnd)
      keyInfo `shouldSatisfy`
        (\ki ->
          (keyInfoUserOAuthSub ki == uid) &&
          ((keyInfoCreationTime ki) > (CreationTime tStart) &&
           (keyInfoCreationTime ki) < (CreationTime tEnd)) &&
          isNothing (keyInfoRevocation ki)
        )

  describe "revokePublicKey tests" $ do
    it "Revoke public key with permissions" $ \orContext -> do
      Just pubKey <- goodRsaPublicKey
      myKeyState <- testAppM orContext $ do
        user <- insertDummies
        keyId <- addPublicKey user pubKey Nothing
        _timeKeyRevoked <- revokePublicKey user keyId
        keyInfo <- getPublicKeyInfo keyId
        pure (keyInfoState keyInfo)
      myKeyState `shouldBe` Revoked

{-- XXX - FIXME!!! Need to catch UnAuthorisedKeyAccess error
    it "Revoke public key without permissions" $ \orContext -> do
      pubKey <- goodRsaPublicKey
      r <- testAppM orContext $ do
        uid <- addUser dummyNewUser {newUserCompany=orgPfx}
        tableUser <- runDb $ getUserByIdQuery uid
        let user = tableToAuthUser . fromJust $ tableUser
        keyId <- addPublicKey user pubKey Nothing

        -- making a fake user
        hackerUid <- addUser $ makeDummyNewUser (EmailAddress "l33t@hacker.com")
        storageHacker <- runDb $ getUserByIdQuery hackerUid
        let hacker = tableToAuthUser . fromJust $ storageHacker

        revokePublicKey hacker keyId
      r `shouldSatisfy` isLeft
      -- r `shouldBe` Left ORT.UnauthorisedKeyAccess
--}

    it "Expired but NOT revoked pub key" $ \orContext -> do
      Just pubKey <- goodRsaPublicKey
      nowish <- getCurrentTime
      -- A delay of 2 seconds is used here because
      -- insertDummies can take up to 1 second to run. See github #257 for more info.
      let smallDelayInSeconds = 2
          nearExpiry = addUTCTime (fromInteger smallDelayInSeconds) nowish
      keyId <- testAppM orContext $ do
        user <- insertDummies
        keyId <- addPublicKey user pubKey (Just . ExpirationTime $ nearExpiry)
        pure keyId
      threadDelay $ fromIntegral $ secondsToMicroseconds smallDelayInSeconds
      myKeyState <- testAppM orContext $ do
        keyInfo <- getPublicKeyInfo keyId
        pure (keyInfoState keyInfo)
      myKeyState `shouldBe` Expired

    it "Expiry date in the future" $ \orContext -> do
      nowish <- getCurrentTime
      let hundredMinutes = 100 * 60
          someTimeLater = addUTCTime hundredMinutes nowish
      Just pubKey <- goodRsaPublicKey
      myKeyState <- testAppM orContext $ do
        user <- insertDummies
        keyId <- addPublicKey user pubKey (Just . ExpirationTime $ someTimeLater)
        keyInfo <- getPublicKeyInfo keyId
        pure (keyInfoState keyInfo)
      myKeyState `shouldBe` InEffect

  describe "Org" $ do
    it "List Org empty" $ \orContext -> do
      orgList <- testAppM orContext (searchOrgs Nothing Nothing Nothing)
        -- myOrgList <-
        -- pure listOrgs
      orgList `shouldBe` []

  describe "Unit test getKeyState" $ do
    it "results in InEffect when both RevocationTime and ExpirationTime are Nothing" $ \_-> do
      now <- liftIO getCurrentTime
      getKeyState now Nothing Nothing `shouldBe` InEffect


    it "results in Revoked when RevocationTime is the same as the comparison time (and there is no ExpirationTime)" $ \_-> do
      now <- getCurrentTime
      getKeyState now (Just $ RevocationTime now)  Nothing `shouldBe` Revoked

    it "results in Revoked when RevocationTime is the before as the comparison time (and there is no ExpirationTime)" $ \_-> do
      now <- getCurrentTime
      let next = nextUTCTime now
      getKeyState next (Just $ RevocationTime now)  Nothing `shouldBe` Revoked

    it "results in InEffect when RevocationTime is the after as the comparison time (and there is no ExpirationTime)" $ \_-> do
      now <- getCurrentTime
      let next = nextUTCTime now
      getKeyState now (Just $ RevocationTime next)  Nothing `shouldBe` InEffect


    it "results in Expired when ExpirationTime is the same as the comparison time (and there is no RevokedTime)" $ \_-> do
      now <- getCurrentTime
      getKeyState now Nothing (Just $ ExpirationTime now) `shouldBe` Expired

    it "results in Expired when ExpirationTime is the before as the comparison time (and there is no RevokedTime)" $ \_-> do
      now <- getCurrentTime
      let next = nextUTCTime now
      getKeyState next Nothing (Just $ ExpirationTime now) `shouldBe` Expired

    it "results in InEffect when ExpirationTime is the after as the comparison time (and there is no RevokedTime)" $ \_-> do
      now <- getCurrentTime
      let next = nextUTCTime now
      getKeyState now Nothing (Just $ ExpirationTime next) `shouldBe` InEffect


    -- For the following tests we enumerate all possibilites and document
    -- afterwards which cases are identical. To reduce notation length we
    -- document using the notation `1` to mean the earliest reference time, `2`
    -- to mean the value epsilon after `1` and `3` to mean the value epsilon
    -- after `2`. We use the positional location in the function call i.e. `123`
    -- means `getKeyState 1 (RevocationTime 2) (ExpirationTime 3)`. Hence `121`
    -- is the same test as `232` (since in both cases the times used are
    -- identical realative to each other).

    it "results in Revoked when RevocationTime and ExpirationTime is the same as the comparison time" $ \_-> do
      now <- getCurrentTime
      getKeyState now (Just $ RevocationTime now) (Just $ ExpirationTime now) `shouldBe` Revoked -- 111, 222, 333

    it "results in Revoked when comparison time is equal to or after the RevocationTime irrespective of what ExpirationTime is" $ \_-> do
      now <- getCurrentTime
      let next = nextUTCTime now
          beyond = nextUTCTime next
      getKeyState now    (Just $ RevocationTime now)  (Just $ ExpirationTime next)   `shouldBe` Revoked -- 112, 113, 223
      getKeyState next   (Just $ RevocationTime now)  (Just $ ExpirationTime now)    `shouldBe` Revoked -- 211, 311, 322
      getKeyState next   (Just $ RevocationTime now)  (Just $ ExpirationTime next)   `shouldBe` Revoked -- 212, 313, 323
      getKeyState next   (Just $ RevocationTime now)  (Just $ ExpirationTime beyond) `shouldBe` Revoked -- 213
      getKeyState next   (Just $ RevocationTime next) (Just $ ExpirationTime now)    `shouldBe` Revoked -- 221, 331, 332
      getKeyState beyond (Just $ RevocationTime now)  (Just $ ExpirationTime next)   `shouldBe` Revoked -- 312
      getKeyState beyond (Just $ RevocationTime next) (Just $ ExpirationTime now)    `shouldBe` Revoked -- 321

    it "results in Expired when the comparison time is equal to or after the ExpirationTime and comparison time is before the RevocationTime" $ \_-> do
      now <- getCurrentTime
      let next = nextUTCTime now
          beyond = nextUTCTime next
      getKeyState now  (Just $ RevocationTime next)   (Just $ ExpirationTime now) `shouldBe` Expired -- 121, 131, 232
      getKeyState next (Just $ RevocationTime beyond) (Just $ ExpirationTime now) `shouldBe` Expired -- 231

    it "results in InEffect when the comparison time is before both RevocationTime and ExpirationTime" $ \_-> do
      now <- getCurrentTime
      let next = nextUTCTime now
          beyond = nextUTCTime next
      getKeyState now (Just $ RevocationTime next)   (Just $ ExpirationTime next)   `shouldBe` InEffect -- 122, 133, 233
      getKeyState now (Just $ RevocationTime next)   (Just $ ExpirationTime beyond) `shouldBe` InEffect -- 123
      getKeyState now (Just $ RevocationTime beyond) (Just $ ExpirationTime next)   `shouldBe` InEffect -- 132



-- *****************************************************************************
-- Test Utility Functions
-- *****************************************************************************

-- | Adds the dummy org and user and returns the user id and auth user.
insertDummies :: AppM ORContextMinimal ORError AuthUser
insertDummies = do
  uid <- addUserOnlyId dummyNewUser
  _orgPrefix <- addOrg uid dummyOrg
  tableUser <- runDb $ getUserByIdQuery uid
  pure (tableUserToAuthUser . fromJust $ tableUser)

-- | Produces the minimal +ve NominalDiffTime (epsilon).
epsNominalDiffTime :: NominalDiffTime
epsNominalDiffTime = succ $ toEnum 0

-- | Produces the next UTCTime after the supplied time (epsilon after the supplied time).
nextUTCTime :: UTCTime -> UTCTime
nextUTCTime = addUTCTime epsNominalDiffTime
