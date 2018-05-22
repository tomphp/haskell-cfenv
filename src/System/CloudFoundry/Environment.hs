{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module System.CloudFoundry.Environment
    ( Application(..)
    , Limits(..)
    , Service(..)
    , credentialString
    , current
    , isRunningOnCf
    , withLabel
    , withName
    , withTag
    ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map

import Control.Monad.Except (liftEither)
import Control.Monad.Trans.Either (EitherT, runEitherT)

import System.CloudFoundry.Environment.Application (Application(..), Limits(..))
import qualified System.CloudFoundry.Environment.Decoder as Decoder
import System.CloudFoundry.Environment.Service
  ( Service(..)
  , Services
  , credentialString
  , withLabel
  , withName
  , withTag
  )

-- | Detect if the application is running as a Cloud Foundry application.
isRunningOnCf :: IO Bool
isRunningOnCf =
    envHasValue "VCAP_APPLICATION"
  where
    envHasValue = lookupEnv >=> return . maybe False isEmpty
    isEmpty = not . (==) "" . dropWhile isSpace

-- | Get the current Cloud Foundry environment.
current :: IO (Either String Application)
current = runEitherT currentT

currentT :: EitherT String IO Application
currentT = do
    home <- stringFromEnv "HOME"
    memoryLimit <- stringFromEnv "MEMORY_LIMIT"
    pwd <- stringFromEnv "PWD"
    port <- numberFromEnv "PORT"
    tmpDir <- stringFromEnv "TMPDIR"
    user <- stringFromEnv "USER"
    services <- servicesFromEnv

    let parser =
          Decoder.vcapApplicationParser
            services
            home
            memoryLimit
            pwd
            port
            tmpDir
            user

    vcapApplication <- stringFromEnv "VCAP_APPLICATION"

    liftEither $ addErrorPrefix $ Decoder.decodeVcapApplication parser vcapApplication
   where
     addErrorPrefix = mapLeft ("VCAP_APPLICATION " ++)

servicesFromEnv :: EitherT String IO Services
servicesFromEnv =
    liftEither . addErrorPrefix . decode =<< lookupEnv' "VCAP_SERVICES"
  where
    decode = maybe (Right Map.empty) Decoder.decodeVcapServices
    addErrorPrefix = mapLeft ("VCAP_SERVICES " ++)

stringFromEnv :: String -> EitherT String IO String
stringFromEnv envName =
  liftEither . maybeToEither (envName ++ " is not set.") =<< lookupEnv' envName

numberFromEnv :: String -> EitherT String IO Int
numberFromEnv envName =
    liftEither . readEither errorMessage =<< stringFromEnv envName
  where
    readEither error value = maybeToEither (errorMessage value) (readMaybe value)
    errorMessage value = envName ++ " must be an integer, got '" ++ value ++ "'."

lookupEnv' :: String -> EitherT String IO (Maybe String)
lookupEnv' = liftIO . lookupEnv

maybeToEither :: e -> Maybe v -> Either e v
maybeToEither error =
  \case
    Just value -> Right value
    Nothing -> Left error

mapLeft :: (e -> e1) -> Either e a -> Either e1 a
mapLeft f (Left error)  = Left $ f error
mapLeft _ (Right value) = Right value