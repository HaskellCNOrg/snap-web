{-# LANGUAGE DeriveDataTypeable #-}

module Models.Internal.Exception where

import           Control.Monad.CatchIO (Exception (..), throw)
import           Control.Monad.Trans   (MonadIO)
import qualified Data.Text             as T
import           Data.Typeable
import           Database.MongoDB      (Failure (..))

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
failureToUE (DocNotFound _) = throw $ UserException "Document not Found."
failureToUE e = throw . UserException $ show e
