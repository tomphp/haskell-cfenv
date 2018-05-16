module CFEnv
    ( Application(..)
    , current
    ) where

import System.Environment (lookupEnv)

data Application = Application
     { home :: String
     } deriving (Eq, Show)

current :: IO (Maybe Application)
current = do
        vcapApplication <- lookupEnv "VCAP_APPLICATION"
        home <- lookupEnv "HOME"

        return $ case (vcapApplication, home) of
            (Nothing, _) -> Nothing
            (_, Nothing) -> Nothing
            (_, _) -> Just Application { home = maybe "" id home }


