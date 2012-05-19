{-# LANGUAGE DeriveDataTypeable         #-}

module Models.Exception where 

import qualified Data.Text as T
import           Control.Monad.CatchIO (throw, Exception(..))
import           Data.Typeable
import           Control.Monad.Trans (MonadIO)

data UserException = UserException String 
                     | PasswordTooShort Int
                     | UserAlreadyExists
                     deriving (Read, Eq, Show, Typeable)

instance Exception UserException

showUE :: UserException -> T.Text
showUE (PasswordTooShort x) = T.pack $ "Password too short, at least " ++ show x
showUE x = T.pack $ show x

throwUE :: (Show s, MonadIO m) => s -> m a
throwUE = throw . UserException . show
