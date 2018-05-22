{-# LANGUAGE QuasiQuotes #-}

module System.CloudFoundry.Environment.DecoderSpec where

import qualified Data.Map.Strict as Map

import Test.Hspec
import Text.RawString.QQ

import qualified System.CloudFoundry.Environment.Application as Application
import qualified System.CloudFoundry.Environment.Decoder as Decoder
import qualified System.CloudFoundry.Environment.Service as Service

spec :: Spec
spec = do
  let servicesInJson =
        Map.singleton
          "cleardb"
          [ Service.Service
                { Service.name = "service_name"
                , Service.label = "service_label"
                , Service.tags = ["tag_a"]
                , Service.plan = "service_plan"
                , Service.credentials =
                    Map.fromList
                      [ ("username", "service_username")
                      , ("password", "service_password")
                      ]
                }
            ]

  describe "decodeVcapApplication" $ do
    let vcapAppParser =
          Decoder.vcapApplicationParser
            servicesInJson
            "/home/userZ"
            "256M"
            "/pwd"
            9000
            "/tmpdir"
            "tom"

    it "returns error for bad JSON" $ do
      let app = Decoder.decodeVcapApplication vcapAppParser "not-json"
      app `shouldBe` Left "Error in $: string"

    it "returns Application for valid JSON" $ do
      let app = Decoder.decodeVcapApplication vcapAppParser vcapApplicationJson
      let expected =
            Application.Application
              { Application.appId = "abc_application_id"
              , Application.applicationUris =
                  ["haskell-test.cfapps.io"]
              , Application.cfApi = "https://api.sys.foundation"
              , Application.home = "/home/userZ"
              , Application.host = "app_host"
              , Application.instanceId = "abc_instance_id"
              , Application.index = 100
              , Application.memoryLimit = "256M"
              , Application.appName = "app_name"
              , Application.pwd = "/pwd"
              , Application.port = 9000
              , Application.tmpDir = "/tmpdir"
              , Application.services = servicesInJson
              , Application.spaceId = "abc_space_id"
              , Application.spaceName = "development"
              , Application.user = "tom"
              , Application.version = "xxx_version"
              , Application.limits =
                  Application.Limits
                    { Application.disk = 1024
                    , Application.fds = 16384
                    , Application.mem = 2048
                    }
              }
      app `shouldBe` Right expected

  describe "vcapServicesDecoder" $ do
    it "returns error for bad JSON" $ do
      let services = Decoder.decodeVcapServices "not-json"
      services `shouldBe` Left "Error in $: string"

    it "returns the services for good JSON" $ do
      let services = Decoder.decodeVcapServices vcapServicesJson
      services `shouldBe` Right servicesInJson


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