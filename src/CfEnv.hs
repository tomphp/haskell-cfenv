{-# LANGUAGE LambdaCase #-}

module CfEnv
  ( Application(..)
  , current
  ) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Application = Application
  { home :: String
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

  return $ mkApplication <$> home
                         <*> memoryLimit
                         <*> pwd
                         <*> portNumber
                         <*> tmpDir
                         <*> user
                         <*> vcapApplication

mkApplication :: String -> String -> String -> Int -> String -> String -> a -> Application
mkApplication home memoryLimit pwd port tmpDir user _ =
  Application { home = home
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
  (\case
    Just value -> Right value
    Nothing    -> Left error)
