{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Envelope
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX
Description : Envelope type used to return responses from a JSON API

This module contains the 'Envelope' type and helper functions. The 'Envelope'
type can be used to wrap responses from a JSON API.

The following is an example of using this package.

First, we will import some needed modules.

>>> import qualified Data.ByteString.Lazy.Char8 as C8
>>> import Data.Aeson (decode, encode)

Let's look at how a success reponse is encoded and decoded.  It is encoded as
an object with a single member: @\"data\"@.

>>> let successEnvelope :: Envelope Text Int = toSuccessEnvelope 3
>>> C8.putStrLn $ encode successEnvelope
{"data":3}
>>> decode "{\"data\":3}" :: Maybe (Envelope Text Int)
Just (EnvelopeSuccess (Success 3))

Now lets look at how an error response is encoded and decoded.  It is encoded
as an object with two members: @\"extra\"@ and @\"error\"@.

>>> let errorEnvelope :: Envelope String Int = toErrEnvelope "example error with DB connection"
>>> C8.putStrLn $ encode errorEnvelope
{"extra":null,"error":"example error with DB connection"}
>>> decode "{\"extra\":\"extratest\",\"error\":\"example error with DB connection\"}" :: Maybe (Envelope String Int)
Just (EnvelopeErr (Err {errErr = "example error with DB connection", errExtra = Just "extratest"}))

The 'Success' and 'Err' types are used within the 'Envelope' type synonym.
-}

module Web.Envelope
    ( Envelope
    , Envelope'(..)
    , Success(..)
    , Err(..)
    , throwEnvelopeErr
    , throwEnvelopeErr'
    , toSuccessEnvelope
    , toErrEnvelope
    ) where

import Prelude

import Control.Monad.Except (MonadError, throwError)
import Control.Applicative ((<|>))
import Data.Aeson
    ( (.=), (.:), FromJSON(..), ToJSON(..), Value(..), object )
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Main type to be used.  Wrapper around responses from an API, mainly used with a JSON API.
--
-- Type synonym around 'Envelope''.
type Envelope e a = Envelope' (Err e) (Success a)

-- | Wrapper around either a success or an error.  Isomorphic to 'Either'.
--
-- The only interesting part of this type is the 'ToJSON' and 'FromJSON'
-- instances.
data Envelope' e a = EnvelopeErr e | EnvelopeSuccess a
    deriving (Data, Eq, Generic, Show, Typeable)

instance (ToJSON e, ToJSON a) => ToJSON (Envelope' e a) where
    toJSON :: Envelope' e a -> Value
    toJSON (EnvelopeErr appErr) = toJSON appErr
    toJSON (EnvelopeSuccess successResp) = toJSON successResp

-- | Tries to parse a successful response.  If you are using the 'Envelope'
-- type synonym, this will use the 'FromJSON' instance for 'Success'.
--
-- If that fails, try to parse an error response.  If you are using the
-- 'Envelope' type synonym, this will use the 'FromJSON' instance for 'Err'.
instance (FromJSON e, FromJSON a) => FromJSON (Envelope' e a) where
    parseJSON :: Value -> Parser (Envelope' e a)
    parseJSON v = EnvelopeSuccess <$> parseJSON v
              <|> EnvelopeErr     <$> parseJSON v
              <|> typeMismatch "Envelope'" v

-- | Newtype wrapper to be able to provide 'ToJSON' and 'FromJSON' instances.
-- Used with 'Envelope'.
newtype Success a = Success a
    deriving (Data, Eq, Generic, Show, Typeable)

instance (ToJSON a) => ToJSON (Success a) where
    toJSON :: Success a -> Value
    toJSON (Success a) = object ["data" .= a]

-- | For @'Success' a@, wrap the @a@ in an object with a @\"data\"@ field.
--
-- The resulting JSON object will look like this:
--
-- @
--  { \"data\": ... }
-- @
instance (FromJSON e) => FromJSON (Success e) where
    parseJSON :: Value -> Parser (Success e)
    parseJSON (Object v) = Success <$> v .: "data"
    parseJSON invalid    = typeMismatch "Success" invalid

-- | Newtype wrapper to be able to provide 'ToJSON' and 'FromJSON' instances.
data Err e = Err { errErr   :: e          -- ^ Actual error information we want to send.
                 , errExtra :: Maybe Text -- ^ Additional error information in plain text.
                 }
    deriving (Data, Eq, Generic, Show, Typeable)

instance (ToJSON e) => ToJSON (Err e) where
    toJSON :: Err e -> Value
    toJSON (Err e extra) = object ["error" .= e, "extra" .= extra]

-- | For @'Err' e@, wrap the @e@ in an object with @\"extra\"@ and @\"error\"@ fields.
--
-- The resulting JSON object will look like this:
--
-- @
--  { \"extra\": ..., \"error\": .... }
-- @
instance (FromJSON e) => FromJSON (Err e) where
    parseJSON :: Value -> Parser (Err e)
    parseJSON (Object v) = Err <$> v .: "error"
                               <*> v .: "extra"
    parseJSON invalid    = typeMismatch "Err" invalid

-- | Wrap an @a@ in a success 'Envelope'.
--
-- >>> toSuccessEnvelope 3 :: Envelope String Int
-- EnvelopeSuccess (Success 3)
toSuccessEnvelope :: a -> Envelope e a
toSuccessEnvelope = EnvelopeSuccess . Success

-- | Wrap an @a@ in an error 'Envelope'.
--
-- >>> toErrEnvelope "there was an error" :: Envelope String Int
-- EnvelopeErr (Err {errErr = "there was an error", errExtra = Nothing})
toErrEnvelope :: e -> Envelope e a
toErrEnvelope e = EnvelopeErr $ Err e Nothing

-- | Throw an @'Err e'@ using 'throwError' in a 'Monad' that implements
-- 'MonadError'.
--
-- If you have @'ExceptT' ('Err' e)@ somewhere inside your monad transformer
-- stacks, this function can be used to throw an error (return early) in a
-- function.
--
-- >>> import Control.Monad.Except (runExcept)
-- >>> throwEnvelopeErr "BAD_ERROR" "a very bad error occurred!" :: Either (Err String) Int
-- Left (Err {errErr = "BAD_ERROR", errExtra = Just "a very bad error occurred!"})
--
-- Here is a longer example using a monad stack.
--
-- > type MyMonadStack = ReaderT Int (ExceptT (Err String) IO)
-- >
-- > doSomething :: Int -> MyMonadStack Int
-- > doSomething int =
-- >     if int < 0
-- >         then
-- >             throwEnvelopeErr "INT_TOO_SMALL" "the integer passed to doSomething is too small"
-- >         else
-- >             return (int + 1)
throwEnvelopeErr :: (MonadError (Err e) m) => e -> Text -> m a
throwEnvelopeErr e text = throwError $ Err e (Just text)

-- | Like 'throwEnvelopeErr' but without providing a message.
--
-- >>> import Control.Monad.Except (runExcept)
-- >>> throwEnvelopeErr "BAD_ERROR" "a very bad error occurred!" :: Either (Err String) Int
-- Left (Err {errErr = "BAD_ERROR", errExtra = Just "a very bad error occurred!"})
throwEnvelopeErr' :: (MonadError (Err e) m) => e -> m a
throwEnvelopeErr' e = throwError $ Err e Nothing
