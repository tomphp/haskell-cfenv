module CFEnvSpec where

import System.Environment (setEnv, unsetEnv)
import Test.Hspec

import CFEnv

spec :: Spec
spec = do
  describe "CFEnv" $ do
    describe "current" $ before
      (do
        setEnv "PORT" "9000"
        setEnv "HOME" "/home/userZ"
        setEnv "MEMORY_LIMIT" "256M"
        setEnv "VCAP_APPLICATION" "{}"
      )
      $ do
      it "returns error if PORT is not set" $ do
        unsetEnv "PORT"
        app <- CFEnv.current
        app `shouldBe` Left "PORT is not set."

      it "returns error if PORT is not valid number" $ do
        setEnv "PORT" "abc"
        app <- CFEnv.current
        app `shouldBe` Left "PORT must be an integer, got 'abc'."

      it "returns error if HOME is not set" $ do
        unsetEnv "HOME"
        app <- CFEnv.current
        app `shouldBe` Left "HOME is not set."

      it "returns error if MEMORY_LIMIT is not set" $ do
        unsetEnv "MEMORY_LIMIT"
        app <- CFEnv.current
        app `shouldBe` Left "MEMORY_LIMIT is not set."

      it "returns error if VCAP_APPLICATION is not set" $ do
        unsetEnv "VCAP_APPLICATION"
        app <- CFEnv.current
        app `shouldBe` Left "VCAP_APPLICATION is not set."

      it "returns Application when the environment is correct" $ do
        app <- CFEnv.current

        app `shouldBe` (Right CFEnv.Application
                              { port = 9000
                              , home = "/home/userZ"
                              , memoryLimit = "256M"
                              })
