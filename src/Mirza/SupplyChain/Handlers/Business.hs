module Mirza.SupplyChain.Handlers.Business where

import           Mirza.SupplyChain.Handlers.Common

import qualified Data.Text                                as T
import qualified Mirza.SupplyChain.QueryUtils             as QU
import qualified Mirza.SupplyChain.StorageBeam            as SB
import           Mirza.SupplyChain.Types                  hiding (KeyInfo (..),
                                                           NewUser (..),
                                                           User (userId),
                                                           UserID)
import qualified Mirza.SupplyChain.Types                  as ST

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           OpenSSL.EVP.PKey                         (SomePublicKey,
                                                           toPublicKey)
import           OpenSSL.PEM                              (readPublicKey,
                                                           writePublicKey)
import           OpenSSL.RSA                              (RSAPubKey, rsaSize)

import           Control.Monad.IO.Class                   (liftIO)



minPubKeySize :: Bit
minPubKeySize = Bit 2048 -- 256 Bytes



getPublicKey ::  SCSApp context err => KeyID -> AppM context err PEM_RSAPubKey
getPublicKey = runDb . getPublicKeyQuery

getPublicKeyQuery :: AsServiceError err =>  KeyID -> DB context err PEM_RSAPubKey
getPublicKeyQuery (KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure (SB.pem_str allKeys)
  case r of
    [k] -> return $ PEMString $ T.unpack k
    _   -> throwing _InvalidKeyID . KeyID $ keyId


getPublicKeyInfo ::  SCSApp context err => KeyID -> AppM context err ST.KeyInfo
getPublicKeyInfo = runDb . getPublicKeyInfoQuery

getPublicKeyInfoQuery :: AsServiceError err => KeyID -> DB context err ST.KeyInfo
getPublicKeyInfoQuery (KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure allKeys

  case r of
    [(SB.Key _ (SB.UserId uId) _  creationTime revocationTime)] ->
       return $ ST.KeyInfo (ST.UserID uId)
                (QU.toEPCISTime creationTime)
                (QU.toEPCISTime <$> revocationTime)
    _ -> throwing _InvalidKeyID . KeyID $ keyId


-- select * from Business;
listBusinesses :: SCSApp context err => AppM context err [Business]
listBusinesses = runDb $ fmap QU.storageToModelBusiness <$> listBusinessesQuery
-- ^ one fmap for Functor AppM, one for Functor []

-- TODO: Write tests
listBusinessesQuery :: DB context err [SB.Business]
listBusinessesQuery = do
  pg $ runSelectReturningList $ select $
      all_ (SB._businesses SB.supplyChainDb)



addPublicKey :: SCSApp context err => ST.User -> PEM_RSAPubKey -> AppM context err KeyID
addPublicKey user pemKey@(PEMString pemStr) = do
  somePubKey <- liftIO $ readPublicKey pemStr
  either (throwing _ServiceError)
         (runDb . addPublicKeyQuery user)
         (checkPubKey somePubKey pemKey)


checkPubKey :: SomePublicKey -> PEM_RSAPubKey-> Either ServiceError RSAPubKey
checkPubKey spKey pemKey =
  maybe (Left $ InvalidRSAKey pemKey)
  (\pubKey ->
    let keySize = rsaSize pubKey in
    -- rsaSize returns size in bytes
    if (Bit $ keySize * 8) < minPubKeySize
      then Left $ InvalidRSAKeySize (Expected minPubKeySize) (Received $ Bit keySize)
      else Right pubKey
  )
  (toPublicKey spKey)


addPublicKeyQuery :: AsServiceError err =>  ST.User -> RSAPubKey -> DB context err KeyID
addPublicKeyQuery (User (ST.UserID uid) _ _)  rsaPubKey = do
  keyId <- QU.generatePk
  timeStamp <- QU.generateTimeStamp
  keyStr <- liftIO $ writePublicKey rsaPubKey
  r <- pg $ runInsertReturningList (SB._keys SB.supplyChainDb) $
        insertValues
        [ SB.Key keyId (SB.UserId uid) (T.pack keyStr) timeStamp Nothing
        ]
  case r of
    [rowId] -> return (KeyID $ SB.key_id rowId)
    _       -> throwing _InvalidKeyID . KeyID $ keyId
