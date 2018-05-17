{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CfEnv
  ( Application(..)
  , current
  ) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

data Application = Application
  { home :: String
  , instanceId :: String
  , memoryLimit :: String
  , pwd :: String
  , port :: Int
  , tmpDir :: String
  , user :: String
  } deriving (Eq, Show)

current :: IO (Either String Application)
current = do
  home <- lookupEnvOrError "HOME"
  memoryLimit <- lookupEnvOrError "MEMORY_LIMIT"
  pwd <- lookupEnvOrError "PWD"
  port <- lookupEnvOrError "PORT"
  tmpDir <- lookupEnvOrError "TMPDIR"
  user <- lookupEnvOrError "USER"
  vcapApplication <- lookupEnvOrError "VCAP_APPLICATION"

  let portNumber = port >>= numberOrError (\p -> "PORT must be an integer, got '" ++ p ++ "'.")

  let application = vcapApplication >>= decodeVcapApp

  return $ setEnvVars <$> home
                      <*> memoryLimit
                      <*> pwd
                      <*> portNumber
                      <*> tmpDir
                      <*> user
                      <*> vcapApplication
                      <*> application

decodeVcapApp :: String -> Either String Application
decodeVcapApp =
  Aeson.eitherDecode . BL.pack

emptyApplication = Application
  { home = ""
  , instanceId = ""
  , memoryLimit = ""
  , pwd = ""
  , port = 8080
  , tmpDir = ""
  , user = ""
  }

instance Aeson.FromJSON Application where
  parseJSON = Aeson.withObject "Application" $ \o -> do
    instanceId <- o Aeson..: "instance_id"

    return emptyApplication { instanceId = instanceId}

setEnvVars :: String -> String -> String -> Int -> String -> String -> String -> Application -> Application
setEnvVars home memoryLimit pwd port tmpDir user vcapApplication application =
  application { home = home
              , memoryLimit = memoryLimit
              , pwd = pwd
              , port = port
              , tmpDir = tmpDir
              , user = user
              }

lookupEnvOrError :: String -> IO (Either String String)
lookupEnvOrError envName = do
  value <- lookupEnv envName
  return $ maybeToEither (envName ++ " is not set.") value

numberOrError :: (String -> String) -> String -> Either String Int
numberOrError error value =
  maybeToEither (error value) (readMaybe value)

maybeToEither :: e -> Maybe v -> Either e v
maybeToEither error =
  \case
    Just value -> Right value
    Nothing    -> Left error
