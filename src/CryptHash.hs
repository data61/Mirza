{-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances  #-}


module CryptHash where

-- import           Data.GS1.Event
import           Model              as M

-- import           Crypto.Hash
import           Data.List.NonEmpty (NonEmpty)
-- import qualified Data.List.NonEmpty   as NonEmpty
-- import qualified Data.Text                  as Txt
-- import qualified Data.Text.Lazy             as TxtL
import           GHC.Generics       (Generic)
--import Crypto.PubKey.RSA
-- import qualified Data.ByteString            as ByteString
-- import qualified Data.ByteString.Char8      as Char8
-- import qualified Data.ByteString.Lazy.Char8 as LChar8
--import qualified Crypto.PubKey.RSA.PSS as PSS
-- import           Codec.Crypto.RSA     (PublicKey (..), verify)
-- import           Data.ByteString.Lazy (fromStrict)


--import Crypto.Hash.SHA256

data BlockchainPackage = BlockchainPackage EventHash (NonEmpty (Signature, UserID))
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
verifySignature :: RSAPublicKey -> ByteString.ByteString -> M.Signature -> Bool
verifySignature (RSAPublicKey n e) event (Signature signature) =
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


getCryptoPublicKey :: RSAPublicKey -> PublicKey
getCryptoPublicKey (RSAPublicKey n e) = PublicKey 128 n e -- FIXME

verifySignature :: RSAPublicKey -> ByteString.ByteString -> M.Signature -> Bool
verifySignature pubKey event (Signature signature) =
    --PSS.verify (PSS.defaultPSSParams SHA256) (getCryptoPublicKey pubKey) event (Char8.pack signature)
    -- note verify in this version uses SHA256
    verify (getCryptoPublicKey pubKey) (fromStrict event) (LChar8.pack signature)
    -}
