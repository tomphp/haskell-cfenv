{-# LANGUAGE RecordWildCards #-}

module System.CloudFoundry.Environment
  ( Application(..)
  , CfEnvError(..)
  , EnvVars.EnvVarError(..)
  , Limits(..)
  , Service(..)
  , current
  , isRunningOnCf
  , module System.CloudFoundry.Environment.Internal.Service
  , module System.CloudFoundry.Environment.Internal.Services
  ) where

import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)
import Data.Char (isSpace)
import System.Environment (lookupEnv)

import qualified System.CloudFoundry.Environment.Internal.EnvVars as EnvVars
import System.CloudFoundry.Environment.Internal.Service
import System.CloudFoundry.Environment.Internal.Services
import System.CloudFoundry.Environment.Internal.Types
import qualified System.CloudFoundry.Environment.Internal.VcapApplicationDecoder as VcapApplication
import qualified System.CloudFoundry.Environment.Internal.VcapServicesDecoder as VcapServices

data CfEnvError = EnvVarError EnvVars.EnvVarError | DecodeError String String deriving (Eq)

instance Exception CfEnvError

instance Show CfEnvError where
  show (EnvVarError error)      = show error
  show (DecodeError name error) = name ++ " " ++ error

-- | Detect if the application is running as a Cloud Foundry application.
isRunningOnCf :: IO Bool
isRunningOnCf =
    envHasValue "VCAP_APPLICATION"
  where
    envHasValue = lookupEnv >=> return . maybe False isEmpty
    isEmpty = not . (==) "" . dropWhile isSpace

-- | Get the current Cloud Foundry environment.
current :: (MonadThrow m, MonadIO m) => m Application
current = do
  envVars <- EnvVars.getEnvVars
  vcapApp <- decodeVcapApplication' (EnvVars.vcapApplication envVars)
  vcapServices <- decodeVcapServices' (EnvVars.vcapServices envVars)

  return $ mkApplication envVars vcapApp vcapServices

decodeVcapApplication' :: (MonadThrow m, MonadIO m) => String -> m VcapApplication.VcapApplication
decodeVcapApplication' =
    eitherToThrow VcapApplication.decode (DecodeError "VCAP_APPLICATION")

decodeVcapServices' :: (MonadThrow m, MonadIO m) => String -> m Services
decodeVcapServices' =
    eitherToThrow VcapServices.decode (DecodeError "VCAP_SERVICES")

eitherToThrow :: (MonadThrow m, MonadIO m, Exception ex) => (input -> Either error output) -> (error -> ex) -> input -> m output
eitherToThrow fn exFn input =
  case fn input of
    Right output -> return output
    Left error   -> throwM $ exFn error

mkApplication :: EnvVars.EnvVars -> VcapApplication.VcapApplication -> Services -> Application
mkApplication envVars vcapApp services =
    Application
      { appId = VcapApplication.appId vcapApp
      , appName = VcapApplication.appName vcapApp
      , applicationUris = VcapApplication.applicationUris vcapApp
      , cfApi = VcapApplication.cfApi vcapApp
      , home = EnvVars.home envVars
      , host = VcapApplication.host vcapApp
      , index = VcapApplication.index vcapApp
      , instanceId = VcapApplication.instanceId vcapApp
      , limits = VcapApplication.limits vcapApp
      , memoryLimit = EnvVars.memoryLimit envVars
      , port = EnvVars.port envVars
      , pwd = EnvVars.pwd envVars
      , services = services
      , spaceId = VcapApplication.spaceId vcapApp
      , spaceName = VcapApplication.spaceName vcapApp
      , tmpDir = EnvVars.tmpDir envVars
      , user = EnvVars.user envVars
      , version = VcapApplication.version vcapApp
      }
