{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module System.CloudFoundry.Environment.Decoder
  ( decodeVcapApplication
  , vcapApplicationParser
  , decodeVcapServices
  ) where

import Control.Monad ((>=>))

import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as BL

import System.CloudFoundry.Environment.Application
import System.CloudFoundry.Environment.Service

decodeVcapApplication :: (A.Value -> AT.Parser Application) -> String -> Either String Application
decodeVcapApplication parser =
  (A.eitherDecode . BL.pack) >=> AT.parseEither parser

vcapApplicationParser :: Services
                      -> String
                      -> String
                      -> String
                      -> Int
                      -> String
                      -> String
                      -> A.Value
                      -> AT.Parser Application
vcapApplicationParser services home memoryLimit pwd port tmpDir user =
  A.withObject "Application" $ \o -> do
    appId <- o .: "application_id"
    applicationUris <- o .: "application_uris"
    cfApi <- o .: "cf_api"
    host <- o .: "host"
    instanceId <- o .: "instance_id"
    index <- o .: "instance_index"
    limits <- o .: "limits"
    appName <- o .: "name"
    spaceId <- o .: "space_id"
    spaceName <- o .: "space_name"
    version <- o .: "version"
    return Application {..}

instance FromJSON Limits

instance FromJSON Service

decodeVcapServices :: String -> Either String Services
decodeVcapServices = A.eitherDecode . BL.pack
