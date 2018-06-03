{-# LANGUAGE LambdaCase, RecordWildCards #-}

module System.CloudFoundry.Environment.Internal.EnvVars
  ( EnvVars(..)
  , getEnvVars
  ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import System.Environment.Extended (eitherLookupEnv, getEnv, getEnvDefault, lookupEnv)

import Control.Error
import Control.Error.Util (note)
import Control.Monad.Except (liftEither)
import Text.Read (readMaybe)

data EnvVars = EnvVars
  { home :: String
  , memoryLimit :: String
  , pwd :: String
  , port :: Int
  , tmpDir :: String
  , user :: String
  }

-- TODO: Test me!!!
getEnvVars :: ExceptT String IO EnvVars
getEnvVars = do
  home <- stringFromEnv "HOME"
  memoryLimit <- stringFromEnv "MEMORY_LIMIT"
  pwd <- stringFromEnv "PWD"
  port <- numberFromEnv "PORT"
  tmpDir <- stringFromEnv "TMPDIR"
  user <- stringFromEnv "USER"
  return EnvVars{..}

-- TODO: Test me!!!
stringFromEnv :: String -> ExceptT String IO String
stringFromEnv = ExceptT . eitherLookupEnv'

-- TODO: Test me!!!
numberFromEnv :: String -> ExceptT String IO Int
numberFromEnv envName = ExceptT $ fmap (>>= readEither) $ eitherLookupEnv' envName
  where
    readEither value = note (errorMessage value) (readMaybe value)
    errorMessage value = envName ++ " must be an integer, got '" ++ value ++ "'."

eitherLookupEnv' :: String -> IO (Either String String)
eitherLookupEnv' envName =
    eitherLookupEnv (envName ++ " is not set.") envName

