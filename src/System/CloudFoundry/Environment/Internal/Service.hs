module System.CloudFoundry.Environment.Internal.Service where

import qualified Data.Map.Strict as Map
import System.CloudFoundry.Environment.Internal.Types

-- | Get a credential string from a service.
credentialString :: String -> Service -> Maybe String
credentialString key = Map.lookup key . credentials