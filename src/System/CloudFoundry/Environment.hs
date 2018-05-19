{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module System.CloudFoundry.Environment
  ( Application(..)
  , Limits (..)
  , current
  ) where

import Control.Monad (join)
import GHC.Generics
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Data.Aeson ((.:), FromJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as BL

data Application = Application
  { appId :: String
  , cfApi :: String
  , home :: String
  , host :: String
  , instanceId :: String
  , index :: Int
  , limits :: Limits
  , memoryLimit :: String
  , name :: String
  , pwd :: String
  , port :: Int
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

instance FromJSON Limits

current :: IO (Either String Application)
current =  do
  home <- stringFromEnv "HOME"
  memoryLimit <- stringFromEnv "MEMORY_LIMIT"
  pwd <- stringFromEnv "PWD"
  port <- numberFromEnv "PORT"
  tmpDir <- stringFromEnv "TMPDIR"
  user <- stringFromEnv "USER"
  vcapApplication <- stringFromEnv "VCAP_APPLICATION"

  let parser = vcapApplicationParser <$> home
                                     <*> memoryLimit
                                     <*> pwd
                                     <*> port
                                     <*> tmpDir
                                     <*> user

  return $ join (decodeVcapApplication <$> parser <*> vcapApplication)

decodeVcapApplication :: (A.Value -> AT.Parser Application) -> String -> Either String Application
decodeVcapApplication parser json =
  mapLeft ("VCAP_APPLICATION " ++)
    (A.eitherDecode (BL.pack json) >>= AT.parseEither parser)

vcapApplicationParser :: String
                      -> String
                      -> String
                      -> Int
                      -> String
                      -> String
                      -> A.Value -> AT.Parser Application
vcapApplicationParser home memoryLimit pwd port tmpDir user =
  A.withObject "Application" $ \o -> do
    appId <- o .: "application_id"
    cfApi <- o .: "cf_api"
    host <- o .: "host"
    instanceId <- o .: "instance_id"
    index <- o .: "instance_index"
    limits <- o .: "limits"
    name <- o .: "name"
    spaceId <- o .: "space_id"
    spaceName <- o .: "space_name"
    version <- o .: "version"

    return Application {..}

stringFromEnv :: String -> IO (Either String String)
stringFromEnv envName = do
  value <- lookupEnv envName
  return $ maybeToEither (envName ++ " is not set.") value

numberFromEnv :: String -> IO (Either String Int)
numberFromEnv envName = do
  string <- stringFromEnv envName
  return $ string >>= numberOrError (\p -> "PORT must be an integer, got '" ++ p ++ "'.")

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
