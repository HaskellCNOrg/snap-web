{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Views.UserForm where

import Control.Applicative ((<$>), (<*>))

import qualified Data.Text as T
import Data.Text (Text)
import Text.Digestive

import Views.Validators

data LoginUser = LoginUser
    { loginName :: Text
    , password :: Text
    } deriving (Show)

-- TODO: fetch message from i10n snaplet.
--
userForm :: Monad m => Form Text m LoginUser
userForm = LoginUser
    <$> "loginName" .: check "loginName is required" requiredValidator (text Nothing)
    <*> "password" .: check "Password is required" requiredValidator (text Nothing)


