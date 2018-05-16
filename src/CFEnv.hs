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

        return $ mkApplication <$> vcapApplication
                               <*> home
                               <*> memoryLimit

mkApplication :: a -> String -> String -> Application
mkApplication _ home memoryLimit =
    Application { home = home
                , memoryLimit = memoryLimit
                }
