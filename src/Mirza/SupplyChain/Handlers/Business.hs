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
