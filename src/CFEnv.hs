module CFEnv
    ( Application(..)
    , current
    ) where

import System.Environment (lookupEnv)

data Application = Application deriving (Eq, Show)

current :: IO (Maybe Application)
current = do
        envVar <- lookupEnv "VCAP_APPLICATION"

        return $ fmap (const Application) envVar
