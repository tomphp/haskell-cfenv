{-# LANGUAGE QuasiQuotes #-}

module System.CloudFoundry.EnvironmentSpec where

import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import System.Environment (setEnv, unsetEnv)

import Test.Hspec
import Text.RawString.QQ

import qualified System.CloudFoundry.Environment as CfEnv

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

vcapServices :: String
vcapServices =
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

setEnvVars :: IO ()
setEnvVars = do
  setEnv "HOME" "/home/userZ"
  setEnv "MEMORY_LIMIT" "256M"
  setEnv "PORT" "9000"
  setEnv "PWD" "/pwd"
  setEnv "TMPDIR" "/tmpdir"
  setEnv "USER" "tom"
  setEnv "VCAP_APPLICATION" vcapApplicationJson
  setEnv "VCAP_SERVICES" vcapServices

spec :: Spec
spec = do
  describe "CfEnv" $ do
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
          let expected =
                CfEnv.Application
                  { CfEnv.appId = "abc_application_id"
                  , CfEnv.applicationUris =
                      ["haskell-test.cfapps.io"]
                  , CfEnv.cfApi = "https://api.sys.foundation"
                  , CfEnv.home = "/home/userZ"
                  , CfEnv.host = "app_host"
                  , CfEnv.instanceId = "abc_instance_id"
                  , CfEnv.index = 100
                  , CfEnv.memoryLimit = "256M"
                  , CfEnv.appName = "app_name"
                  , CfEnv.pwd = "/pwd"
                  , CfEnv.port = 9000
                  , CfEnv.tmpDir = "/tmpdir"
                  , CfEnv.services =
                      Map.fromList
                        [ ("cleardb",
                            [ CfEnv.Service
                                { CfEnv.name = "service_name"
                                , CfEnv.label = "service_label"
                                , CfEnv.tags = ["tag_a"]
                                , CfEnv.plan = "service_plan"
                                , CfEnv.credentials =
                                    Map.fromList [ ("username", "service_username")
                                                 , ("password", "service_password")
                                                 ]
                                }
                            ]
                          )
                        ]
                  , CfEnv.spaceId = "abc_space_id"
                  , CfEnv.spaceName = "development"
                  , CfEnv.user = "tom"
                  , CfEnv.version = "xxx_version"
                  , CfEnv.limits =
                      CfEnv.Limits
                        { CfEnv.disk = 1024
                        , CfEnv.fds = 16384
                        , CfEnv.mem = 2048
                        }
                  }
          app `shouldBe` Right expected
    describe "credentialString" $ do
      let service = CfEnv.Service
            { CfEnv.name = "service_name"
            , CfEnv.label = "service_label"
            , CfEnv.tags = ["tag_a"]
            , CfEnv.plan = "service_plan"
            , CfEnv.credentials = Map.singleton "the-key" "the-value"
            }
      it "returns nothing if there is no matching string" $ do
        CfEnv.credentialString "unknown" service `shouldBe` Nothing
      it "returns the string if it is found" $ do
        CfEnv.credentialString "the-key" service `shouldBe` Just "the-value"
    context "for Service searching functions" $ do
      let serviceA =
            CfEnv.Service
              { CfEnv.name = "name-a"
              , CfEnv.label = "label-a"
              , CfEnv.tags = ["good_tag", "ignore_tag"]
              , CfEnv.plan = "plan-a"
              , CfEnv.credentials = Map.empty
              }
      let serviceB =
            CfEnv.Service
              { CfEnv.name = "name-b"
              , CfEnv.label = "label-b"
              , CfEnv.tags = ["ignore_tag"]
              , CfEnv.plan = "plan-b"
              , CfEnv.credentials = Map.empty
              }
      let serviceC =
            CfEnv.Service
              { CfEnv.name = "name-c"
              , CfEnv.label = "label-c"
              , CfEnv.tags = ["good_tag"]
              , CfEnv.plan = "plan-c"
              , CfEnv.credentials = Map.empty
              }
      let services = Map.fromList
                      [ ("relational-db", [serviceA, serviceB])
                      , ("message-queue" , [serviceC])
                      ]
      describe "withTag" $ do
        it "returns an empty list if no matching services are found" $ do
          CfEnv.withTag "bad_tag" services `shouldBe` []
        it "returns the services with matching tags" $ do
          CfEnv.withTag "good_tag" services `shouldBe` [serviceC , serviceA]
      describe "withName" $ do
        it "returns nothing if there is no service with the given name" $ do
          CfEnv.withName "unknown" services `shouldBe` Nothing
        it "returns the service if it exists" $ do
          CfEnv.withName "name-b" services `shouldBe` Just serviceB
      describe "withLabel" $ do
        it "returns an empty list if there is no service with the given label" $ do
          CfEnv.withLabel "unknown" services `shouldBe` []
        it "returns the services with that labe" $ do
          CfEnv.withLabel "relational-db" services `shouldBe` [serviceA, serviceB]