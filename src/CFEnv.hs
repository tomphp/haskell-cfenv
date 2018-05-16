module CFEnv
    ( Application(..)
    , current
    ) where

import System.Environment (lookupEnv)

data Application = Application
     { home :: String
     , memoryLimit :: String
     } deriving (Eq, Show)

current :: IO (Either String Application)
current = do
        home <- lookupEnv "HOME"
        memoryLimit <- lookupEnv "MEMORY_LIMIT"
        vcapApplication <- lookupEnv "VCAP_APPLICATION"

        let homeEither = maybeToEither "HOME is not set." home
        let memoryLimitEither = maybeToEither "MEMORY_LIMIT is not set." memoryLimit
        let vcapApplicationEither = maybeToEither "VCAP_APPLICATION is not set." vcapApplication

        return $ mkApplication <$> vcapApplicationEither
                               <*> homeEither
                               <*> memoryLimitEither

mkApplication :: a -> String -> String -> Application
mkApplication _ home memoryLimit =
    Application { home = home
                , memoryLimit = memoryLimit
                }

maybeToEither :: e -> Maybe v -> Either e v
maybeToEither error maybe = case maybe of
                                Just value -> Right value
                                Nothing    -> Left error
