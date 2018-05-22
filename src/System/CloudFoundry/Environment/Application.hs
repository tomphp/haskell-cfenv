{-# LANGUAGE DeriveGeneric #-}

module System.CloudFoundry.Environment.Application
    ( Application(..), Limits(..) ) where

import GHC.Generics

import System.CloudFoundry.Environment.Service

data Application = Application
    { appId           :: String
    , applicationUris :: [String]
    , cfApi           :: String
    , home            :: String
    , host            :: String
    , instanceId      :: String
    , index           :: Int
    , limits          :: Limits
    , memoryLimit     :: String
    , appName         :: String
    , pwd             :: String
    , port            :: Int
    , services        :: Services
    , spaceId         :: String
    , spaceName       :: String
    , tmpDir          :: String
    , user            :: String
    , version         :: String
    } deriving (Eq, Show)

data Limits = Limits
    { disk :: Int
    , fds  :: Int
    , mem  :: Int
    } deriving (Eq, Show, Generic)