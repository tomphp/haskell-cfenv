{-# LANGUAGE LambdaCase #-}

module CFEnv
  ( Application(..)
  , current
  ) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Application = Application
  { port :: Int
  , home :: String
  , memoryLimit :: String
  , pwd :: String
  , tmpDir :: String
  , user :: String
  } deriving (Eq, Show)

current :: IO (Either String Application)
current = do
  port <- lookupEnvOrError "PORT"
  home <- lookupEnvOrError "HOME"
  memoryLimit <- lookupEnvOrError "MEMORY_LIMIT"
  pwd <- lookupEnvOrError "PWD"
  tmpDir <- lookupEnvOrError "TMPDIR"
  user <- lookupEnvOrError "USER"
  vcapApplication <- lookupEnvOrError "VCAP_APPLICATION"

  let portNumber = port >>= numberOrError (\p -> "PORT must be an integer, got '" ++ p ++ "'.")

  return $ mkApplication <$> portNumber
                         <*> home
                         <*> memoryLimit
                         <*> pwd
                         <*> tmpDir
                         <*> user
                         <*> vcapApplication

mkApplication :: Int -> String -> String -> String -> String -> String -> a -> Application
mkApplication port home memoryLimit pwd tmpDir user _ =
  Application { port = port
              , home = home
              , memoryLimit = memoryLimit
              , pwd = pwd
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
