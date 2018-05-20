{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module System.CloudFoundry.Environment
    ( Application(..)
    , Limits (..)
    , Service (..)
    , current
    , isRunningOnCf
    ) where

import Control.Monad ((>=>), join)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import GHC.Generics
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Data.Aeson ((.:), FromJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as BL

data Application = Application
    { appId :: String
    , applicationUris :: [String]
    , cfApi :: String
    , home :: String
    , host :: String
    , instanceId :: String
    , index :: Int
    , limits :: Limits
    , memoryLimit :: String
    , appName :: String
    , pwd :: String
    , port :: Int
    , services :: [Service]
    , spaceId :: String
    , spaceName :: String
    , tmpDir :: String
    , user :: String
    , version :: String
    } deriving (Eq, Show)

data Limits = Limits
    { disk :: Int
    , fds :: Int
    , mem :: Int
    } deriving (Eq, Show, Generic)

data Service = Service
    { name :: String
    , label :: String
    , tags :: [String]
    , plan :: String
    -- , credentials :: Map String ???
    } deriving (Eq, Show, Generic)


isRunningOnCf :: IO Bool
isRunningOnCf = envHasValue "VCAP_APPLICATION"
  where envHasValue = lookupEnv >=> return . fromMaybe False . fmap isEmpty
        isEmpty = not . (==) "" . dropWhile isSpace

current :: IO (Either String Application)
current =  do
    home <- stringFromEnv "HOME"
    memoryLimit <- stringFromEnv "MEMORY_LIMIT"
    pwd <- stringFromEnv "PWD"
    port <- numberFromEnv "PORT"
    tmpDir <- stringFromEnv "TMPDIR"
    user <- stringFromEnv "USER"

    vcapServices <- lookupEnv "VCAP_SERVICES"
    let services = decodeVcapServices <$> vcapServices

    let parser = vcapApplicationParser <$> (fromMaybe (Right []) services)
                                       <*> home
                                       <*> memoryLimit
                                       <*> pwd
                                       <*> port
                                       <*> tmpDir
                                       <*> user

    vcapApplication <- stringFromEnv "VCAP_APPLICATION"

    return $ join (decodeVcapApplication <$> parser <*> vcapApplication)

decodeVcapApplication :: (A.Value -> AT.Parser Application) -> String -> Either String Application
decodeVcapApplication parser = addErrorPrefix . parseJson
  where parseJson = (A.eitherDecode . BL.pack) >=> AT.parseEither parser
        addErrorPrefix = mapLeft ("VCAP_APPLICATION " ++)

vcapApplicationParser :: [Service]
                      -> String
                      -> String
                      -> String
                      -> Int
                      -> String
                      -> String
                      -> A.Value -> AT.Parser Application
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
decodeVcapServices = addErrorPrefix . A.eitherDecode . BL.pack
  where addErrorPrefix = mapLeft ("VCAP_SERVICES " ++)

stringFromEnv :: String -> IO (Either String String)
stringFromEnv envName = do
    value <- lookupEnv envName
    return $ maybeToEither (envName ++ " is not set.") value

numberFromEnv :: String -> IO (Either String Int)
numberFromEnv envName = do
    string <- stringFromEnv envName
    return $ string >>= numberOrError (\p -> envName ++ " must be an integer, got '" ++ p ++ "'.")

numberOrError :: (String -> String) -> String -> Either String Int
numberOrError error value =
    maybeToEither (error value) (readMaybe value)

maybeToEither :: e -> Maybe v -> Either e v
maybeToEither error =
    \case
        Just value -> Right value
        Nothing    -> Left error

mapLeft :: (e -> e1) -> Either e a -> Either e1 a
mapLeft f (Left error)  = Left $ f error
mapLeft _ (Right value) = Right value
