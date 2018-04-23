{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


module CryptHash where

import           Data.List.NonEmpty (NonEmpty)
import           GHC.Generics       (Generic)
import qualified Model              as M

data BlockchainPackage = BlockchainPackage M.EventHash (NonEmpty (M.Signature, M.UserID))
  deriving (Show, Read, Eq, Generic)

{-
hexSha2_256 :: ByteString.ByteString -> M.EventHash
hexSha2_256 bs = EventHash (show  (hash bs :: Digest SHA256))

hashEvent :: Event -> M.EventHash
hashEvent event = hexSha256  (TxtL.unpack $ TxtL.toStrict $ encodeToLazyText event)

hashJsonTxt :: Txt.Text -> M.EventHash
hashJsonTxt json = hexSha2_256 (Txt.unpack $ json)
-}

{-
sha256 :: Char8.ByteString -> Digest SHA256
sha256 = hash
verifySignature :: PEM_RSAPubKey -> ByteString.ByteString -> M.Signature -> Bool
verifySignature (PEM_RSAPubKey n e) event (Signature signature) =
    PSS.verify (PSS.defaultPSSParams SHA256) pubKey event (Char8.pack signature)
    where
      pubKey = PublicKey 128 n e --FIXME
verifySignature :: ByteString.ByteString -- ^ message signature
                -> Digest
                -> RSAPubKey             -- ^ public key to verify the signature
                -> ByteString.ByteString -- ^ input string to verify
                -> VerifyStatus          -- ^ the result of the verification
--verifyBS  :: PublicKey key	 => Digest -> ByteString -> key ->  ByteString -> IO VerifyStatus
verifySignature = verifyBS


getCryptoPublicKey :: PEM_RSAPubKey -> PublicKey
getCryptoPublicKey (PEM_RSAPubKey n e) = PublicKey 128 n e -- FIXME

verifySignature :: PEM_RSAPubKey -> ByteString.ByteString -> M.Signature -> Bool
verifySignature pubKey event (Signature signature) =
    --PSS.verify (PSS.defaultPSSParams SHA256) (getCryptoPublicKey pubKey) event (Char8.pack signature)
    -- note verify in this version uses SHA256
    verify (getCryptoPublicKey pubKey) (fromStrict event) (LChar8.pack signature)
    -}
