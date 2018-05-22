{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module System.CloudFoundry.Environment.Service
  ( Service(..)
  , Services
  , credentialString
  , withTag
  , withLabel
  , withName
  )
  where

import Control.Monad (join)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map.Strict (Map)
import GHC.Generics
import qualified Data.Map.Strict as Map

data Service = Service
  { name  :: String
  , label :: String
  , tags  :: [String]
  , plan  :: String
  , credentials :: Map String String
  } deriving (Eq, Show, Generic)

type Services = Map String [Service]

-- | Get a credential string from a service.
credentialString :: String -> Service -> Maybe String
credentialString key = Map.lookup key . credentials

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