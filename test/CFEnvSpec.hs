module CFEnvSpec where

import System.Environment (setEnv, unsetEnv)
import Test.Hspec

import CFEnv

spec :: Spec
spec = do
  describe "CFEnv" $ do
    context "current" $ do
      it "returns Nothing if HOME is not set" $ do
        unsetEnv "HOME"
        setEnv "MEMORY_LIMIT" "512M"
        setEnv "VCAP_APPLICATION" "{}"

        app <- CFEnv.current

        app `shouldBe` Left "HOME is not set."

      it "returns Nothing if MEMORY_LIMIT is not set" $ do
        setEnv "HOME" "/home/userY"
        unsetEnv "MEMORY_LIMIT"
        setEnv "VCAP_APPLICATION" "{}"

        app <- CFEnv.current

        app `shouldBe` Left "MEMORY_LIMIT is not set."

      it "returns Nothing if VCAP_APPLICATION is not set." $ do
        setEnv "HOME" "/home/userX"
        unsetEnv "VCAP_APPLICATION"

        app <- CFEnv.current

        app `shouldBe` Left "VCAP_APPLICATION is not set."

      it "returns Application if VCAP_APPLICATION is set" $ do
        setEnv "HOME" "/home/userZ"
        setEnv "MEMORY_LIMIT" "256M"
        setEnv "VCAP_APPLICATION" "{}"

        app <- CFEnv.current

        app `shouldBe` (Right CFEnv.Application
                              { home = "/home/userZ"
                              , memoryLimit = "256M"
                              })
