module CFEnvSpec where

import System.Environment (setEnv, unsetEnv)
import Test.Hspec

import CFEnv

spec :: Spec
spec = do
  describe "CFEnv" $ do
    context "current" $ do
      it "returns Nothing if VCAP_APPLICATION is not set" $ do
        setEnv "HOME" "/home/userX"
        unsetEnv "VCAP_APPLICATION"

        app <- CFEnv.current

        app `shouldBe` Nothing

      it "returns Nothing if HOME is not set" $ do
        unsetEnv "HOME"
        setEnv "VCAP_APPLICATION" "{}"

        app <- CFEnv.current

        app `shouldBe` Nothing

      it "returns Application if VCAP_APPLICATION is set" $ do
        setEnv "HOME" "/home/userZ"
        setEnv "VCAP_APPLICATION" "{}"

        app <- CFEnv.current

        app `shouldBe` (Just CFEnv.Application { home = "/home/userZ" })
