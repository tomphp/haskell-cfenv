module System.CloudFoundry.Environment.ServiceSpec where

import qualified Data.Map.Strict as Map

import Test.Hspec

import qualified System.CloudFoundry.Environment.Service as Service

spec :: Spec
spec = do
  describe "credentialString" $ do
    let service = Service.Service
          { Service.name = "service_name"
          , Service.label = "service_label"
          , Service.tags = ["tag_a"]
          , Service.plan = "service_plan"
          , Service.credentials = Map.singleton "the-key" "the-value"
          }

    it "returns nothing if there is no matching string" $ do
      Service.credentialString "unknown" service `shouldBe` Nothing

    it "returns the string if it is found" $ do
      Service.credentialString "the-key" service `shouldBe` Just "the-value"

  context "for Service searching functions" $ do
    let serviceA =
          Service.Service
            { Service.name = "name-a"
            , Service.label = "label-a"
            , Service.tags = ["good_tag", "ignore_tag"]
            , Service.plan = "plan-a"
            , Service.credentials = Map.empty
            }
    let serviceB =
          Service.Service
            { Service.name = "name-b"
            , Service.label = "label-b"
            , Service.tags = ["ignore_tag"]
            , Service.plan = "plan-b"
            , Service.credentials = Map.empty
            }
    let serviceC =
          Service.Service
            { Service.name = "name-c"
            , Service.label = "label-c"
            , Service.tags = ["good_tag"]
            , Service.plan = "plan-c"
            , Service.credentials = Map.empty
            }
    let services = Map.fromList
                    [ ("relational-db", [serviceA, serviceB])
                    , ("message-queue" , [serviceC])
                    ]

    describe "withTag" $ do
      it "returns an empty list if no matching services are found" $ do
        Service.withTag "bad_tag" services `shouldBe` []

      it "returns the services with matching tags" $ do
        Service.withTag "good_tag" services `shouldBe` [serviceC , serviceA]

    describe "withName" $ do
      it "returns nothing if there is no service with the given name" $ do
        Service.withName "unknown" services `shouldBe` Nothing

      it "returns the service if it exists" $ do
        Service.withName "name-b" services `shouldBe` Just serviceB

    describe "withLabel" $ do
      it "returns an empty list if there is no service with the given label" $ do
        Service.withLabel "unknown" services `shouldBe` []

      it "returns the services with that labe" $ do
        Service.withLabel "relational-db" services `shouldBe` [serviceA, serviceB]