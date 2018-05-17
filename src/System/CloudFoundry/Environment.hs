{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module System.CloudFoundry.Environment
  ( Application(..)
  , current
  ) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

data Application = Application
  { appId :: String
  , cfApi :: String
  , home :: String
  , host :: String
  , instanceId :: String
  , index :: Int
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

  let application = mapLeft (\e -> "VCAP_APPLICATION " ++ e) (vcapApplication >>= decodeVcapApp)

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
  { appId = ""
  , cfApi = ""
  , home = ""
  , host = ""
  , instanceId = ""
  , index = 0
  , memoryLimit = ""
  , name = ""
  , pwd = ""
  , port = 8080
  , tmpDir = ""
  , spaceId = ""
  , spaceName = ""
  , user = ""
  , version = ""
  }

instance Aeson.FromJSON Application where
  parseJSON = Aeson.withObject "Application" $ \o -> do
    appId <- o Aeson..: "application_id"
    cfApi <- o Aeson..: "cf_api"
    host <- o Aeson..: "host"
    instanceId <- o Aeson..: "instance_id"
    index <- o Aeson..: "instance_index"
    name <- o Aeson..: "name"
    spaceId <- o Aeson..: "space_id"
    spaceName <- o Aeson..: "space_name"
    version <- o Aeson..: "version"

    return emptyApplication
      {  appId = appId
      , cfApi = cfApi
      , host = host
      , instanceId = instanceId
      , index = index
      , name = name
      , spaceId = spaceId
      , spaceName = spaceName
      , version = version
      }

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

mapLeft :: (e -> e1) -> Either e a -> Either e1 a
mapLeft f (Left error)  = Left $ f error
mapLeft _ (Right value) = Right value
