{-# LANGUAGE DeriveGeneric #-}

module System.CloudFoundry.Environment.Internal.Types where

import Control.Exception.Safe (Exception)
import Data.Map.Strict (Map)
import GHC.Generics

import qualified Data.Aeson as Aeson

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

data Service = Service
  { name  :: String
  , label :: String
  , tags  :: [String]
  , plan  :: String
  , credentials :: Map String String
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Service

type Services = Map String [Service]

data CfEnvError = DecodeError String String
                | NotInteger String String deriving (Eq)

instance Exception CfEnvError

instance Show CfEnvError where
  show (DecodeError name error) = name ++ " " ++ error
  show (NotInteger envName value) = envName ++ " must be an integer, got '" ++ value ++ "'."
