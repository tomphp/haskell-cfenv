{-# LANGUAGE QuasiQuotes #-}

module System.CloudFoundry.EnvironmentSpec where

import System.Environment (setEnv, unsetEnv)

import Test.Hspec
import Text.RawString.QQ

import qualified System.CloudFoundry.Environment as CfEnv

vcapApplicationJson :: String
vcapApplicationJson = [r|{
	"instance_id": "abc_instance_id",
	"application_id": "abc_application_id",
  "application_uris": [
    "haskell-test.cfapps.io"
  ],
	"instance_index": 100,
	"name": "app_name",
	"port": 1080,
	"host": "app_host",
	"version": "xxx_version",
	"application_uris": ["appname.apps.foundation"],
	"space_id": "abc_space_id",
	"space_name": "development",
	"cf_api": "https://api.sys.foundation",
	"limits": {
	  "disk": 1024,
    "fds": 16384,
    "mem": 2048
	}
}|]

spec :: Spec
spec = do
  describe "CfEnv" $ do
    describe "current" $ before
      (do
        setEnv "HOME" "/home/userZ"
        setEnv "MEMORY_LIMIT" "256M"
        setEnv "PORT" "9000"
        setEnv "PWD" "/pwd"
        setEnv "TMPDIR" "/tmpdir"
        setEnv "USER" "tom"
        setEnv "VCAP_APPLICATION" vcapApplicationJson
      )
      $ do
        it "returns error if HOME is not set" $ do
          unsetEnv "HOME"
          app <- CfEnv.current
          app `shouldBe` Left "HOME is not set."

        it "returns error if MEMORY_LIMIT is not set" $ do
          unsetEnv "MEMORY_LIMIT"
          app <- CfEnv.current
          app `shouldBe` Left "MEMORY_LIMIT is not set."

        it "returns error if PWD is not set" $ do
          unsetEnv "PWD"
          app <- CfEnv.current
          app `shouldBe` Left "PWD is not set."

        it "returns error if PORT is not set" $ do
          unsetEnv "PORT"
          app <- CfEnv.current
          app `shouldBe` Left "PORT is not set."

        it "returns error if PORT is not valid number" $ do
          setEnv "PORT" "abc"
          app <- CfEnv.current
          app `shouldBe` Left "PORT must be an integer, got 'abc'."

        it "returns error if TMPDIR is not set" $ do
          unsetEnv "TMPDIR"
          app <- CfEnv.current
          app `shouldBe` Left "TMPDIR is not set."

        it "returns error if USER is not set" $ do
          unsetEnv "USER"
          app <- CfEnv.current
          app `shouldBe` Left "USER is not set."

        it "returns error if VCAP_APPLICATION is not set" $ do
          unsetEnv "VCAP_APPLICATION"
          app <- CfEnv.current
          app `shouldBe` Left "VCAP_APPLICATION is not set."

        it "returns error if VCAP_APPLICATION bad JSON" $ do
          setEnv "VCAP_APPLICATION" "not-json"
          app <- CfEnv.current
          app `shouldBe` Left "VCAP_APPLICATION Error in $: string"

        it "returns Application when the environment is correct" $ do
          app <- CfEnv.current

          app `shouldBe` Right CfEnv.Application
                                { CfEnv.appId = "abc_application_id"
                                , CfEnv.applicationUris = ["haskell-test.cfapps.io"]
                                , CfEnv.cfApi = "https://api.sys.foundation"
                                , CfEnv.home = "/home/userZ"
                                , CfEnv.host = "app_host"
                                , CfEnv.instanceId = "abc_instance_id"
                                , CfEnv.index = 100
                                , CfEnv.memoryLimit = "256M"
                                , CfEnv.name = "app_name"
                                , CfEnv.pwd = "/pwd"
                                , CfEnv.port = 9000
                                , CfEnv.tmpDir = "/tmpdir"
                                , CfEnv.spaceId = "abc_space_id"
                                , CfEnv.spaceName = "development"
                                , CfEnv.user = "tom"
                                , CfEnv.version = "xxx_version"
                                , CfEnv.limits = CfEnv.Limits
                                  { CfEnv.disk = 1024
                                  , CfEnv.fds  = 16384
                                  , CfEnv.mem  = 2048
                                  }
                                }
