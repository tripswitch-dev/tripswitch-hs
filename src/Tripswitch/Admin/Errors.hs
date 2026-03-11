{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Tripswitch.Admin.Errors
-- Description : Admin API error taxonomy.
module Tripswitch.Admin.Errors
  ( -- * Error type
    APIError (..)
  , AdminErrorKind (..)

    -- * Predicates
  , isNotFound
  , isUnauthorized
  , isForbidden
  , isConflict
  , isRateLimited
  , isValidation
  , isServerFault
  , isTransport
  ) where

import Control.Exception (Exception (..))
import Data.Text (Text)

-- | The kind of admin API error.
data AdminErrorKind
  = -- | 400/422: validation error.
    ErrValidation
  | -- | 401: unauthorized.
    ErrUnauthorized
  | -- | 403: forbidden.
    ErrForbidden
  | -- | 404: not found.
    ErrNotFound
  | -- | 409: conflict.
    ErrConflict
  | -- | 429: rate limited.
    ErrRateLimited
  | -- | 5xx: server fault.
    ErrServerFault
  | -- | Transport error (connection, DNS, etc.).
    ErrTransport
  deriving stock (Eq, Show)

-- | An error from the TripSwitch admin API.
data APIError = APIError
  { aeKind :: !AdminErrorKind
  , aeStatusCode :: !Int
  , aeMessage :: !Text
  , aeRetryAfter :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

instance Exception APIError

-- | Is this a 404 Not Found error?
isNotFound :: APIError -> Bool
isNotFound = (== ErrNotFound) . aeKind

-- | Is this a 401 Unauthorized error?
isUnauthorized :: APIError -> Bool
isUnauthorized = (== ErrUnauthorized) . aeKind

-- | Is this a 403 Forbidden error?
isForbidden :: APIError -> Bool
isForbidden = (== ErrForbidden) . aeKind

-- | Is this a 409 Conflict error?
isConflict :: APIError -> Bool
isConflict = (== ErrConflict) . aeKind

-- | Is this a 429 Rate Limited error?
isRateLimited :: APIError -> Bool
isRateLimited = (== ErrRateLimited) . aeKind

-- | Is this a validation error (400/422)?
isValidation :: APIError -> Bool
isValidation = (== ErrValidation) . aeKind

-- | Is this a server fault (5xx)?
isServerFault :: APIError -> Bool
isServerFault = (== ErrServerFault) . aeKind

-- | Is this a transport error?
isTransport :: APIError -> Bool
isTransport = (== ErrTransport) . aeKind
