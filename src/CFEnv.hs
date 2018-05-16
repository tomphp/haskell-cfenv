module CFEnv
    ( Application(..)
    , current
    ) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Application = Application
     { port :: Int
     , home :: String
     , memoryLimit :: String
     } deriving (Eq, Show)

current :: IO (Either String Application)
current = do
        port <- lookupEnv "PORT"
        home <- lookupEnv "HOME"
        memoryLimit <- lookupEnv "MEMORY_LIMIT"
        vcapApplication <- lookupEnv "VCAP_APPLICATION"

        let portEither = case port of
                                Just p -> maybeToEither ("PORT must be an integer, got '" ++ p ++ "'.") (readMaybe p)
                                Nothing -> Left "PORT is not set."

        let homeEither = maybeToEither "HOME is not set." home
        let memoryLimitEither = maybeToEither "MEMORY_LIMIT is not set." memoryLimit
        let vcapApplicationEither = maybeToEither "VCAP_APPLICATION is not set." vcapApplication

        return $ mkApplication <$> portEither
                               <*> homeEither
                               <*> memoryLimitEither
                               <*> vcapApplicationEither

mkApplication :: Int -> String -> String -> a -> Application
mkApplication port home memoryLimit _ =
    Application { port = port
                , home = home
                , memoryLimit = memoryLimit
                }

maybeToEither :: e -> Maybe v -> Either e v
maybeToEither error maybe = case maybe of
                                Just value -> Right value
                                Nothing    -> Left error
