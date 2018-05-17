{-# LANGUAGE QuasiQuotes #-}

module System.CloudFoundry.EnvironmentSpec where

import System.Environment (setEnv, unsetEnv)

import Test.Hspec
import Text.RawString.QQ

import qualified System.CloudFoundry.Environment as CfEnv

vcapApplicationJson = [r|{
	"instance_id": "abc_instance_id",
	"application_id": "abc_application_id",
	"instance_index": 100,
	"name": "app_name",
	"port": 1080,
	"host": "app_host",
	"version": "xxx_version",
	"application_uris": ["appname.apps.foundation"],
	"space_id": "abc_space_id",
	"space_name": "development",
	"cf_api": "https://api.sys.foundation",
	"limits": {}
}|]

{-
 "VCAP_APPLICATION": {
  "application_id": "ef1d4861-0e4f-4b10-b662-678d183e1772",
  "application_name": "haskell-test",
  "application_uris": [
   "haskell-test.cfapps.io"
  ],
  "application_version": "922bd52a-2203-4dc9-b596-4c4ed947ef4b",
  "cf_api": "https://api.run.pivotal.io",
  "limits": {
   "disk": 1024,
   "fds": 16384,
   "mem": 2048
  },
  "name": "haskell-test",
  "space_id": "95b9361a-4864-4aab-931d-5094b862fa0e",
  "space_name": "development",
  "uris": [
   "haskell-test.cfapps.io"
  ],
  "users": null,
  "version": "922bd52a-2203-4dc9-b596-4c4ed947ef4b"
 }
-}

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
                                }
