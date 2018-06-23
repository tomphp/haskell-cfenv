{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Environment.Extended
  ( module System.Environment
  , getEnvDefault
  ) where

import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)
import System.Environment

getEnvDefault :: String -> String -> IO String
getEnvDefault def =
  lookupEnv >=> return . fromMaybe def
