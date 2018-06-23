{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Environment.Extended
  ( module System.Environment
  , eitherLookupEnv
  , lookupEnvE
  , getEnvDefault
  ) where

import Control.Error.Util (note)
import Control.Monad ((>=>))
import Control.Exception.Safe (Exception, MonadThrow, SomeException, throwM, try, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import System.Environment

data EnvVarException = EnvVarNotSet String deriving (Show)
instance Exception EnvVarException

eitherLookupEnv :: a -> String -> IO (Either a String)
eitherLookupEnv errorMsg envName =
    fmap (addError errorMsg) (try (lookupEnvE envName))
  where
    addError :: a -> Either EnvVarException String -> Either a String
    addError msg = mapLeft (const msg)

lookupEnvE :: (MonadIO m, MonadThrow m) => String -> m String
lookupEnvE envName = do
    liftIO $ lookupEnv envName >>= orThrow (EnvVarNotSet envName)
  where
    orThrow :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
    orThrow exception =
      \case
        Just x  -> return x
        Nothing -> throwM exception

getEnvDefault :: String -> String -> IO String
getEnvDefault def =
  lookupEnv >=> return . fromMaybe def

mapLeft :: (e -> e1) -> Either e a -> Either e1 a
mapLeft f (Left error)  = Left $ f error
mapLeft _ (Right value) = Right value