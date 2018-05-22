{-# LANGUAGE QuasiQuotes #-}

module System.CloudFoundry.EnvironmentSpec where

import Data.Either (isRight)
import System.Environment (setEnv, unsetEnv)

import Test.Hspec
import Text.RawString.QQ

import qualified System.CloudFoundry.Environment as CfEnv
import qualified System.CloudFoundry.Environment.Application as Application
import qualified System.CloudFoundry.Environment.Decoder as Decoder

spec :: Spec
spec = do
  describe "isRunningOnCf" $ do
    it "returns true if VCAP_APPLICATION has a value" $ do
      setEnv "VCAP_APPLICATION" "{}"
      runningOnCf <- CfEnv.isRunningOnCf
      runningOnCf `shouldBe` True

    it "returns true if VCAP_APPLICATION is not set" $ do
      unsetEnv "VCAP_APPLICATION"
      runningOnCf <- CfEnv.isRunningOnCf
      runningOnCf `shouldBe` False

    it "returns true if VCAP_APPLICATION is empty" $ do
      setEnv "VCAP_APPLICATION" ""
      runningOnCf <- CfEnv.isRunningOnCf
      runningOnCf `shouldBe` False

    it "returns true if VCAP_APPLICATION is only whitespace" $ do
      setEnv "VCAP_APPLICATION" "    "
      runningOnCf <- CfEnv.isRunningOnCf
      runningOnCf `shouldBe` False

  describe "current" $
    before setEnvVars $ do
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

      it "returns Application if VCAP_SERVICES is not set" $ do
        unsetEnv "VCAP_SERVICES"
        app <- CfEnv.current
        app `shouldSatisfy` isRight

      it "returns error if VCAP_SERVICES bad JSON" $ do
        setEnv "VCAP_SERVICES" "not-json"
        app <- CfEnv.current
        app `shouldBe` Left "VCAP_SERVICES Error in $: string"

      it "returns Application when the environment is correct" $ do
        app <- CfEnv.current
        app `shouldBe` applicationFromJson

setEnvVars :: IO ()
setEnvVars = do
  setEnv "HOME" "/home/userZ"
  setEnv "MEMORY_LIMIT" "256M"
  setEnv "PORT" "9000"
  setEnv "PWD" "/pwd"
  setEnv "TMPDIR" "/tmpdir"
  setEnv "USER" "tom"
  setEnv "VCAP_APPLICATION" vcapApplicationJson
  setEnv "VCAP_SERVICES" vcapServicesJson

vcapApplicationJson :: String
vcapApplicationJson =
  [r|{
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

vcapServicesJson :: String
vcapServicesJson =
  [r|
    {
      "cleardb": [{
        "name": "service_name",
        "label": "service_label",
        "tags": ["tag_a"],
        "plan": "service_plan",
        "credentials": {
          "username": "service_username",
          "password": "service_password"
        }
      }]
    }
  |]

applicationFromJson :: Either String Application.Application
applicationFromJson = do
  services <- Decoder.decodeVcapServices vcapServicesJson

  Decoder.decodeVcapApplication
    (Decoder.vcapApplicationParser
      services
      "/home/userZ"
      "256M"
      "/pwd"
      9000
      "/tmpdir"
      "tom")
    vcapApplicationJson