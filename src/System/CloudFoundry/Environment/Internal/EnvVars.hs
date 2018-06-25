{-# LANGUAGE RecordWildCards #-}

module System.CloudFoundry.Environment.Internal.EnvVars
  ( EnvVars(..)
  , getEnvVars
  ) where

import Control.Monad ((>=>))
import Control.Exception (throw)
import Data.Maybe (fromMaybe)
import System.Environment (getEnv, lookupEnv)

import Text.Read (readMaybe)

import System.CloudFoundry.Environment.Internal.Types (CfEnvError(NotInteger))

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

getEnvVars :: IO EnvVars
getEnvVars = do
    home <- getEnv "HOME"
    memoryLimit <- getEnv "MEMORY_LIMIT"
    pwd <- getEnv "PWD"
    port <- numberFromEnv "PORT"
    tmpDir <- getEnv "TMPDIR"
    user <- getEnv "USER"
    vcapApplication <- getEnv "VCAP_APPLICATION"
    vcapServices <- getEnvDefault "{}" "VCAP_SERVICES"
    return EnvVars{..}

stringToInt :: String -> String -> IO Int
stringToInt envName str =
  case readMaybe str of
    Just int -> return int
    Nothing  -> throw $ NotInteger envName str

numberFromEnv :: String -> IO Int
numberFromEnv envName =
    envVarValue envName >>= toInt
  where
    envVarValue = getEnv
    toInt       = stringToInt envName

getEnvDefault :: String -> String -> IO String
getEnvDefault def =
  lookupEnv >=> return . fromMaybe def