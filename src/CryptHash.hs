{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE CPP                        #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}


module CryptHash where

import Model as M
import Data.GS1.Event

import qualified Data.Text.Lazy as TxtL
import qualified Data.Text as Txt
import Data.List.NonEmpty(NonEmpty(..))
import GHC.Generics       (Generic)
import qualified Data.List.NonEmpty as NonEmpty
import Crypto.Hash
import Crypto.PubKey.RSA
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Crypto.PubKey.RSA.PSS as PSS

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

sha256 :: Char8.ByteString -> Digest SHA256
sha256 = hash

verifySignature :: RSAPublicKey -> ByteString.ByteString -> M.Signature -> Bool
verifySignature (RSAPublicKey n e) event (Signature signature) = PSS.verify (PSS.defaultPSSParams SHA256) pubKey event (Char8.pack signature)
  where
    pubKey = PublicKey 128 n e --FIXME


