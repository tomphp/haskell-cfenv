{-# LANGUAGE DeriveGeneric #-}

module System.CloudFoundry.Environment.Internal.Types where

import Control.Exception (Exception)
import Data.Map.Strict (Map)
import GHC.Generics

import qualified Data.Aeson as Aeson

-- | A representation of the Cloud Foundry environment.
--   This is returned from 'current'.
data Application = Application
  { appId           :: String
  , applicationUris :: [String]
  , cfApi           :: String
  , home            :: String
  , host            :: String
  , instanceId      :: String
  , index           :: Int
  , limits          :: Limits
  , memoryLimit     :: String
  , appName         :: String
  , pwd             :: String
  , port            :: Int
  , services        :: Services
  , spaceId         :: String
  , spaceName       :: String
  , tmpDir          :: String
  , user            :: String
  , version         :: String
  } deriving (Eq, Show)

data Limits = Limits
  { disk :: Int
  , fds  :: Int
  , mem  :: Int
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Limits

-- | Description of a bound service. Use `credentialString` to extract the
--   connection details.
data Service = Service
  { name  :: String
  , label :: String
  , tags  :: [String]
  , plan  :: String
  , credentials :: Map String String
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Service

-- | A collection of 'Service' instances. 'withTag', 'withName' and 'withLabel' can be
--   used to find the specific services that you want.
newtype Services = Services (Map String [Service]) deriving (Eq, Show)

-- | Exceptions which are raised from this package.
data CfEnvError
    -- | Thrown when a JSON decode failed for either the VCAP_APPLICATION or
    --   VCAP_SERIVCES environment variables.
    = DecodeError String String
    -- | Thrown when an environment variable which is meant to contain an
    --   integer contains invalid characters.
    | NotInteger String String deriving (Eq)

instance Exception CfEnvError

instance Show CfEnvError where
    show (DecodeError envName errorMsg) = envName ++ " " ++ errorMsg
    show (NotInteger envName value) = envName ++ " must be an integer, got '" ++ value ++ "'."
