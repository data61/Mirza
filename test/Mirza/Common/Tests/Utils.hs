{-# LANGUAGE FlexibleContexts #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Tests.Utils
  (
    unsafeMkEmailAddress
  ) where

import           Data.Maybe          (fromJust)

import           Data.ByteString     as BS

import           Text.Email.Validate (EmailAddress, emailAddress)

-- | Only use this with hardcodes email addresses that are guaranteed to return
-- a ``Just``
unsafeMkEmailAddress :: BS.ByteString -> EmailAddress
unsafeMkEmailAddress = fromJust . emailAddress
