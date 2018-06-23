{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables #-}

module System.CloudFoundry.Environment.Internal.EnvVars
  ( EnvVars(..)
  , EnvVarError(..)
  , getEnvVars
  ) where

import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Environment.Extended (getEnv, getEnvDefault)

import Control.Error
import Text.Read (readMaybe)

data EnvVarError = NotSet String | NotInteger String String deriving (Eq)
instance Exception EnvVarError

instance Show EnvVarError where
  show (NotSet envName)           = envName ++ " is not set."
  show (NotInteger envName value) = envName ++ " must be an integer, got '" ++ value ++ "'."

data EnvVars = EnvVars
  { home :: String
  , memoryLimit :: String
  , pwd :: String
  , port :: Int
  , tmpDir :: String
  , user :: String
  , vcapApplication :: String
  , vcapServices :: String
  }

getEnvVars :: (MonadThrow m, MonadIO m) => m EnvVars
getEnvVars = do
    home <- getEnv' "HOME"
    memoryLimit <- getEnv' "MEMORY_LIMIT"
    pwd <- getEnv' "PWD"
    port <- numberFromEnv "PORT"
    tmpDir <- getEnv' "TMPDIR"
    user <- getEnv' "USER"
    vcapApplication <- getEnv' "VCAP_APPLICATION"
    vcapServices <- liftIO $ getEnvDefault "{}" "VCAP_SERVICES"
    return EnvVars{..}
  where
    getEnv' = liftIO . getEnv

stringToInt :: MonadThrow m => String -> String -> m Int
stringToInt envName str =
  case readMaybe str of
    Just int -> return int
    Nothing  -> throwM $ NotInteger envName str

numberFromEnv :: (MonadThrow m, MonadIO m) => String -> m Int
numberFromEnv envName =
    envVarValue envName >>= toInt
  where
    envVarValue = liftIO . getEnv
    toInt       = stringToInt envName
