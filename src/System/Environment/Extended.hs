module System.Environment.Extended
  ( module System.Environment
  , eitherLookupEnv
  , getEnvDefault
  ) where

import Control.Error.Util (note)
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)
import System.Environment

eitherLookupEnv :: a -> String -> IO (Either a String)
eitherLookupEnv errorMsg envName =
    lookupEnv envName >>= return . note errorMsg

getEnvDefault :: String -> String -> IO String
getEnvDefault def =
  lookupEnv >=> return . fromMaybe def