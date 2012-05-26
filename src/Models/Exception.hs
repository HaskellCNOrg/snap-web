{-# LANGUAGE DeriveDataTypeable         #-}

module Models.Exception where 

import qualified Data.Text as T
import           Control.Monad.CatchIO (throw, Exception(..))
import           Database.MongoDB (Failure(..))
import           Data.Typeable
import           Control.Monad.Trans (MonadIO)
--import           Control.Monad.Error.Class (Error)

data UserException = UserException String 
                     | PasswordTooShort Int
                     | UserAlreadyExists
                     deriving (Read, Eq, Show, Typeable)

instance Exception UserException

showUE :: UserException -> T.Text
showUE (PasswordTooShort x) = T.pack $ "Password too short, at least " ++ show x
showUE x = T.pack $ show x

-- | Simply transform any Showable Exception to String and wrap into @UserException@
-- 
throwUE :: (Show s, MonadIO m) => s -> m a
throwUE = throw . UserException . show

-- | Transform MongoDB @Failure@ to customiable @UserException@.
-- 
failureToUE :: (MonadIO m) => Failure -> m a
failureToUE (DocNotFound s) = throw $ UserException "Document not Found."
failureToUE e = throw . UserException $ show e
