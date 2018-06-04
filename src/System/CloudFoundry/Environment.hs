{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module System.CloudFoundry.Environment
  ( Application(..)
  , Limits(..)
  , Service(..)
  , credentialString
  , current
  , isRunningOnCf
  , withLabel
  , withName
  , withTag

  , decodeVcapServices
  , decodeVcapApplication
  , vcapApplicationParser
  , EnvVars(EnvVars)
  ) where

import Control.Monad ((>=>), join)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Generics
import qualified Data.Map.Strict as Map
import System.Environment.Extended (eitherLookupEnv, lookupEnv, getEnvDefault)

import Control.Error
import Control.Monad.Except (liftEither, liftIO)

import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified System.CloudFoundry.Environment.Internal.EnvVars as EV
import System.CloudFoundry.Environment.Internal.EnvVars (EnvVars(EnvVars))
import System.CloudFoundry.Environment.Internal.Types
import System.CloudFoundry.Environment.Internal.VcapApplicationDecoder as VcapApp
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
    decodeVcapApplication = mapLeft (DecodeError "VCAP_APPLICATION") . VcapApp.decode
    decodeVcapServices = mapLeft (DecodeError "VCAP_SERVICES") . VcapServices.decode

mkApplication :: EV.EnvVars -> VcapApp.VcapApplication -> Services  -> Application
mkApplication envVars vcapApp services =
    Application
      { appId = VcapApp.appId vcapApp
      , appName = VcapApp.appName vcapApp
      , applicationUris = VcapApp.applicationUris vcapApp
      , cfApi = VcapApp.cfApi vcapApp
      , home = EV.home envVars
      , host = VcapApp.host vcapApp
      , index = VcapApp.index vcapApp
      , instanceId = VcapApp.instanceId vcapApp
      , limits = VcapApp.limits vcapApp
      , memoryLimit = EV.memoryLimit envVars
      , port = EV.port envVars
      , pwd = EV.pwd envVars
      , services = services
      , spaceId = VcapApp.spaceId vcapApp
      , spaceName = VcapApp.spaceName vcapApp
      , tmpDir = EV.tmpDir envVars
      , user = EV.user envVars
      , version = VcapApp.version vcapApp
      }

mapLeft :: (e -> e1) -> Either e a -> Either e1 a
mapLeft f (Left error)  = Left $ f error
mapLeft _ (Right value) = Right value

-- TODO: Kill below

decodeVcapApplication :: (A.Value -> AT.Parser Application) -> String -> Either String Application
decodeVcapApplication parser =
  eitherDecodeString >=> AT.parseEither parser

vcapApplicationParser :: Services -> EV.EnvVars -> A.Value -> AT.Parser Application
vcapApplicationParser services envVars =
  A.withObject "Application" $ \o -> do
    appId <- o .: "application_id"
    applicationUris <- o .: "application_uris"
    cfApi <- o .: "cf_api"
    host <- o .: "host"
    instanceId <- o .: "instance_id"
    index <- o .: "instance_index"
    limits <- o .: "limits"
    appName <- o .: "name"
    spaceId <- o .: "space_id"
    spaceName <- o .: "space_name"
    version <- o .: "version"

    let home = EV.home envVars
    let memoryLimit = EV.memoryLimit envVars
    let pwd = EV.pwd envVars
    let port = EV.port envVars
    let tmpDir = EV.tmpDir envVars
    let user = EV.user envVars

    return Application {..}

decodeVcapServices :: String -> Either String Services
decodeVcapServices = eitherDecodeString

eitherDecodeString :: (FromJSON a) => String -> Either String a
eitherDecodeString = A.eitherDecode . BL.pack
