{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.CloudFoundry.Environment
    ( Application(..)
    , Limits(..)
    , Service(..)
    , current
    , isRunningOnCf
    ) where

import           Control.Monad              (join, (>=>))
import           Control.Monad.IO.Class     (liftIO)
import           Data.Char                  (isSpace)
import           Data.Maybe                 (fromMaybe)
import           GHC.Generics
import           System.Environment         (lookupEnv)
import           Text.Read                  (readMaybe)

import           Control.Monad.Except       (liftEither)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Aeson                 (FromJSON, (.:))
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as AT
import qualified Data.ByteString.Lazy.Char8 as BL

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
    , services        :: [Service]
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

data Service = Service
    { name  :: String
    , label :: String
    , tags  :: [String]
    , plan  :: String
    -- , credentials :: Map String ???
    } deriving (Eq, Show, Generic)

isRunningOnCf :: IO Bool
isRunningOnCf =
    envHasValue "VCAP_APPLICATION"
  where
    envHasValue = lookupEnv >=> return . maybe False isEmpty
    isEmpty = not . (==) "" . dropWhile isSpace

current :: IO (Either String Application)
current = runEitherT currentT

currentT :: EitherT String IO Application
currentT = do
    home <- stringFromEnv "HOME"
    memoryLimit <- stringFromEnv "MEMORY_LIMIT"
    pwd <- stringFromEnv "PWD"
    port <- numberFromEnv "PORT"
    tmpDir <- stringFromEnv "TMPDIR"
    user <- stringFromEnv "USER"
    services <- servicesFromEnv

    let parser =
            vcapApplicationParser
                services
                home
                memoryLimit
                pwd
                port
                tmpDir
                user

    vcapApplication <- stringFromEnv "VCAP_APPLICATION"

    liftEither $ decodeVcapApplication parser vcapApplication

servicesFromEnv :: EitherT String IO [Service]
servicesFromEnv =
    liftEither . maybe (Right []) decodeVcapServices =<< lookupEnv' "VCAP_SERVICES"

stringFromEnv :: String -> EitherT String IO String
stringFromEnv envName =
    liftEither . maybeToEither (envName ++ " is not set.") =<< lookupEnv' envName

numberFromEnv :: String -> EitherT String IO Int
numberFromEnv envName =
    liftEither . readEither errorMessage =<< stringFromEnv envName
  where
    readEither error value = maybeToEither (errorMessage value) (readMaybe value)
    errorMessage value = envName ++ " must be an integer, got '" ++ value ++ "'."

lookupEnv' :: String -> EitherT String IO (Maybe String)
lookupEnv' = liftIO . lookupEnv

decodeVcapApplication :: (A.Value -> AT.Parser Application) -> String -> Either String Application
decodeVcapApplication parser =
    addErrorPrefix . parseJson
  where
    parseJson = (A.eitherDecode . BL.pack) >=> AT.parseEither parser
    addErrorPrefix = mapLeft ("VCAP_APPLICATION " ++)

vcapApplicationParser ::
       [Service]
    -> String
    -> String
    -> String
    -> Int
    -> String
    -> String
    -> A.Value
    -> AT.Parser Application
vcapApplicationParser services home memoryLimit pwd port tmpDir user =
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
        return Application {..}

instance FromJSON Limits

instance FromJSON Service

decodeVcapServices :: String -> Either String [Service]
decodeVcapServices =
    addErrorPrefix . A.eitherDecode . BL.pack
  where
    addErrorPrefix = mapLeft ("VCAP_SERVICES " ++)

maybeToEither :: e -> Maybe v -> Either e v
maybeToEither error =
    \case
        Just value -> Right value
        Nothing -> Left error

mapLeft :: (e -> e1) -> Either e a -> Either e1 a
mapLeft f (Left error)  = Left $ f error
mapLeft _ (Right value) = Right value
