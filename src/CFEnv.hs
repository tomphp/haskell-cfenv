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
        port <- lookupEnvOrError "PORT" "PORT is not set."
        home <- lookupEnvOrError "HOME" "HOME is not set."
        memoryLimit <- lookupEnvOrError "MEMORY_LIMIT" "MEMORY_LIMIT is not set."
        vcapApplication <- lookupEnvOrError "VCAP_APPLICATION" "VCAP_APPLICATION is not set."

        let portNumber = port >>= numberOrError (\p -> "PORT must be an integer, got '" ++ p ++ "'.")

        return $ mkApplication <$> portNumber
                               <*> home
                               <*> memoryLimit
                               <*> vcapApplication

mkApplication :: Int -> String -> String -> a -> Application
mkApplication port home memoryLimit _ =
    Application { port = port
                , home = home
                , memoryLimit = memoryLimit
                }

lookupEnvOrError :: String -> String -> IO (Either String String)
lookupEnvOrError envName error = do
        value <- lookupEnv envName
        return $ maybeToEither error value

numberOrError :: (String -> String) -> String -> Either String Int
numberOrError error value =
    maybeToEither (error value) (readMaybe value)

maybeToEither :: e -> Maybe v -> Either e v
maybeToEither error maybe = case maybe of
                                Just value -> Right value
                                Nothing    -> Left error
