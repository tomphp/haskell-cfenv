module System.CloudFoundry.Environment.Internal.Services
  ( withLabel
  , withName
  , withTag
  ) where

import Control.Monad ((>=>), join)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)

import System.CloudFoundry.Environment.Internal.Types

-- | Get all services which have the provided tag.
withTag :: String -> Services -> [Service]
withTag searchTag = filter (elem searchTag . tags) . allServices

-- | Get the service by name.
withName :: String -> Services -> Maybe Service
withName searchName = listToMaybe . filter ((== searchName) . name) . allServices

-- | Get the services by label.
withLabel :: String -> Services -> [Service]
withLabel searchLabel = fromMaybe [] . Map.lookup searchLabel

allServices :: Services -> [Service]
allServices = join . Map.elems