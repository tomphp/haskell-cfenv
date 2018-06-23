{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module System.CloudFoundry.Environment
  ( Application(..)
  , EnvVars(EnvVars)
  , Limits(..)
  , Service(..)
  , credentialString
  , current
  , isRunningOnCf
  , withLabel
  , withName
  , withTag
  ) where

import Control.Monad ((>=>), join)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Generics
import qualified Data.Map.Strict as Map
import System.Environment.Extended (lookupEnv, getEnvDefault)

import Control.Error
import Control.Monad.Except (liftEither, liftIO)

import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified System.CloudFoundry.Environment.Internal.EnvVars as EV
import System.CloudFoundry.Environment.Internal.EnvVars (EnvVars(EnvVars))
import System.CloudFoundry.Environment.Internal.Types
import System.CloudFoundry.Environment.Internal.VcapApplicationDecoder as VcapApplication
import System.CloudFoundry.Environment.Internal.VcapServicesDecoder as VcapServices

data CfEnvError = EnvVarError EV.EnvVarError | DecodeError String String

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
current :: IO (Either String Application)
current = runExceptT $ withExceptT show currentT

-- | Get a credential string from a service.
credentialString :: String -> Service -> Maybe String
credentialString key = Map.lookup key . credentials

-- Services functions ------------------

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

currentT :: ExceptT CfEnvError IO Application
currentT = do
    envVars <- getEnvVars
    vcapApp <- liftEither $ decodeVcapApplication (EV.vcapApplication envVars)
    vcapServices <- liftEither $ decodeVcapServices (EV.vcapServices envVars)

    return $ mkApplication envVars vcapApp vcapServices
  where
    getEnvVars = withExceptT EnvVarError EV.getEnvVars
    decodeVcapApplication = mapLeft (DecodeError "VCAP_APPLICATION") . VcapApplication.decode
    decodeVcapServices = mapLeft (DecodeError "VCAP_SERVICES") . VcapServices.decode

mkApplication :: EV.EnvVars -> VcapApplication.VcapApplication -> Services  -> Application
mkApplication envVars vcapApp services =
    Application
      { appId = VcapApplication.appId vcapApp
      , appName = VcapApplication.appName vcapApp
      , applicationUris = VcapApplication.applicationUris vcapApp
      , cfApi = VcapApplication.cfApi vcapApp
      , home = EV.home envVars
      , host = VcapApplication.host vcapApp
      , index = VcapApplication.index vcapApp
      , instanceId = VcapApplication.instanceId vcapApp
      , limits = VcapApplication.limits vcapApp
      , memoryLimit = EV.memoryLimit envVars
      , port = EV.port envVars
      , pwd = EV.pwd envVars
      , services = services
      , spaceId = VcapApplication.spaceId vcapApp
      , spaceName = VcapApplication.spaceName vcapApp
      , tmpDir = EV.tmpDir envVars
      , user = EV.user envVars
      , version = VcapApplication.version vcapApp
      }

mapLeft :: (e -> e1) -> Either e a -> Either e1 a
mapLeft f (Left error)  = Left $ f error
mapLeft _ (Right value) = Right value