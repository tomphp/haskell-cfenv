{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables #-}

module System.CloudFoundry.Environment.Internal.EnvVars
  ( EnvVars(..)
  , EnvVarError(..)
  , getEnvVars
  ) where

import Control.Exception.Safe (IOException, Handler(..), Exception, MonadThrow, throwM, catches)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import System.Environment.Extended (eitherLookupEnv, getEnv, getEnvDefault, lookupEnv)

import Control.Error
import Control.Error.Util (note)
import Control.Monad.Except (liftEither)
import Text.Read (readMaybe)

data EnvVarError = NotSet String | NotInteger String String
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

-- TODO: Test me!!!
getEnvVars :: ExceptT EnvVarError IO EnvVars
getEnvVars = do
  home <- stringFromEnv "HOME"
  memoryLimit <- stringFromEnv "MEMORY_LIMIT"
  pwd <- stringFromEnv "PWD"
  port <- numberFromEnv "PORT"
  tmpDir <- stringFromEnv "TMPDIR"
  user <- stringFromEnv "USER"
  vcapApplication <- stringFromEnv "VCAP_APPLICATION"
  vcapServices <- liftIO $ getEnvDefault "{}" "VCAP_SERVICES"
  return EnvVars{..}

-- TODO: Test me!!!
stringFromEnv :: String -> ExceptT EnvVarError IO String
stringFromEnv = ExceptT . eitherLookupEnv'

stringToInt :: MonadThrow m => String -> String -> m Int
stringToInt envName str =
  case readMaybe str of
    Just int -> return int
    Nothing  -> throwM $ NotInteger envName str

numberFromEnv' :: (MonadThrow m, MonadIO m) => String -> m Int
numberFromEnv' envName =
    envVarValue envName >>= toInt
  where
    envVarValue = liftIO . getEnv
    toInt       = stringToInt envName

-- TODO: Test me!!!
numberFromEnv :: String -> ExceptT EnvVarError IO Int
numberFromEnv envName =
    ExceptT $ numberFromEnv'' envName `catches` [ Handler $ (\(ex :: IOException) -> return $ Left $ NotSet envName)
                                                , Handler $ (\(ex :: EnvVarError) -> return $ Left ex)
                                                ]
  where
    numberFromEnv'' envName = fmap Right $ numberFromEnv' envName
    readEither value = note (errorMessage value) (readMaybe value)
    errorMessage value = NotInteger envName value

eitherLookupEnv' :: String -> IO (Either EnvVarError String)
eitherLookupEnv' envName =
    eitherLookupEnv (NotSet envName) envName

