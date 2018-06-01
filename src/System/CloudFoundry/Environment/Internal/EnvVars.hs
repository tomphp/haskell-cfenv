{-# LANGUAGE LambdaCase, RecordWildCards #-}

module System.CloudFoundry.Environment.Internal.EnvVars
  ( EnvVars(..)
  , getEnvVars
  , numberFromEnv
  , stringFromEnv
  , stringFromEnvWithDefault
  ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import System.Environment (getEnv, lookupEnv)

import Control.Monad.Except (liftEither)
import Control.Monad.Trans.Either (EitherT, newEitherT)
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
getEnvVars :: EitherT String IO EnvVars
getEnvVars = do
    home <- stringFromEnv "HOME"
    memoryLimit <- stringFromEnv "MEMORY_LIMIT"
    pwd <- stringFromEnv "PWD"
    port <- numberFromEnv "PORT"
    tmpDir <- stringFromEnv "TMPDIR"
    user <- stringFromEnv "USER"
    return EnvVars{..}

-- TODO: Test me!!!
stringFromEnv :: String -> EitherT String IO String
stringFromEnv = newEitherT . eitherLookupEnv

-- TODO: Test me!!!
stringFromEnvWithDefault :: String -> String -> EitherT String IO String
stringFromEnvWithDefault def = newEitherT . fmap Right . getEnvDefault "{}"

-- TODO: Test me!!!
numberFromEnv :: String -> EitherT String IO Int
numberFromEnv envName = newEitherT $ fmap (>>= readEither) $ eitherLookupEnv envName
  where
    readEither value = maybeToEither (errorMessage value) (readMaybe value)
    errorMessage value = envName ++ " must be an integer, got '" ++ value ++ "'."

eitherLookupEnv :: String -> IO (Either String String)
eitherLookupEnv envName =
    lookupEnv envName >>= return . maybeToEither (notFoundMessage envName)
  where
    notFoundMessage = (++ " is not set.")

getEnvDefault :: String -> String -> IO String
getEnvDefault def =
  lookupEnv >=> return . fromMaybe def

maybeToEither :: e -> Maybe v -> Either e v
maybeToEither error =
  \case
    Just value -> Right value
    Nothing -> Left error
