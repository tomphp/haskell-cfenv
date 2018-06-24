module System.CloudFoundry.Environment
  ( Application(..)
  , CfEnvError(..)
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

-- |Detect if the application is running as a Cloud Foundry application.
isRunningOnCf :: IO Bool
isRunningOnCf =
    envHasValue "VCAP_APPLICATION"
  where
    envHasValue = lookupEnv >=> return . maybeHasValue
    maybeHasValue = maybe False hasValue
    hasValue = not . isEmptyString . trimLeft
    trimLeft = dropWhile isSpace
    isEmptyString = (==) ""

-- |Get the current Cloud Foundry environment.
current :: (MonadThrow m, MonadIO m) => m Application
current = do
    envVars <- EnvVars.getEnvVars
    vcapApp <- decodeVcapApplication (EnvVars.vcapApplication envVars)
    vcapServices <- decodeVcapServices (EnvVars.vcapServices envVars)

    return $ mkApplication envVars vcapApp vcapServices
  where
    decodeVcapApplication =
      eitherToThrow VcapApplication.decode (DecodeError "VCAP_APPLICATION")

    decodeVcapServices =
      eitherToThrow VcapServices.decode (DecodeError "VCAP_SERVICES")

eitherToThrow :: (MonadThrow m, MonadIO m, Exception ex) => (input -> Either error output) -> (error -> ex) -> input -> m output
eitherToThrow fn exFn input =
  case fn input of
    Right output  -> return output
    Left errorMsg -> throwM $ exFn errorMsg

mkApplication :: EnvVars.EnvVars -> VcapApplication.VcapApplication -> Services -> Application
mkApplication envVars vcapApp vcapServices =
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
      , services = vcapServices
      , spaceId = VcapApplication.spaceId vcapApp
      , spaceName = VcapApplication.spaceName vcapApp
      , tmpDir = EnvVars.tmpDir envVars
      , user = EnvVars.user envVars
      , version = VcapApplication.version vcapApp
      }
