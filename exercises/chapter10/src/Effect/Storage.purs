module Effect.Storage where

import Prelude
import Data.Argonaut (Json)
import Effect (Effect)

foreign import setItem :: String -> String -> Effect Unit

foreign import getItem :: String -> Effect Json

-- (Easy) Write a wrapper for the removeItem method on the localStorage object, and
-- add your foreign function to the Effect.Storage module.

foreign import removeItem :: String -> Effect Unit

-- (Easy) Write a wrapper for the confirm method on the JavaScript Window object,
-- and add your foreign function to the Effect.Alert module.

foreign import confirm :: String -> Effect Unit
