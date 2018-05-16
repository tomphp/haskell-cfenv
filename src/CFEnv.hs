module CFEnv
    ( Application(..)
    , current
    ) where

import System.Environment (lookupEnv)

data Application = Application
     { home :: String
     , memoryLimit :: String
     } deriving (Eq, Show)

current :: IO (Maybe Application)
current = do
        home <- lookupEnv "HOME"
        memoryLimit <- lookupEnv "MEMORY_LIMIT"
        vcapApplication <- lookupEnv "VCAP_APPLICATION"

        return $ case (vcapApplication, home, memoryLimit) of
            (Nothing, _, _) -> Nothing
            (_, Nothing, _) -> Nothing
            (_, _, Nothing) -> Nothing
            _ -> Just Application { home = maybe "" id home
                                  , memoryLimit = maybe "" id memoryLimit
                                  }


