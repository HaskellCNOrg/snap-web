{-# LANGUAGE OverloadedStrings #-}
module Views.UserForm where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Text.Digestive

import Views.Validators
import Models.User


-- | FIXME: Need a better design to get message from i18n snaplet.
-- 
signinForm :: Monad m => (Text,Text) -> Form Text m LoginUser
signinForm (a,b) = LoginUser
    <$> "loginName"      .: check a requiredValidator (text Nothing)
    <*> "password"       .: check b requiredValidator (text Nothing)
    <*> "repeatPassword" .: text Nothing

signupForm :: Monad m => (Text,Text) -> Form Text m LoginUser
signupForm (a,b) = check "Input password must be same" samePasswordValidator $ 
    LoginUser
    <$> "loginName"       .: check a requiredValidator (text Nothing)
    <*> "password"        .: check b requiredValidator (text Nothing)
    <*> "repeatPassword"  .: text Nothing

samePasswordValidator :: LoginUser -> Bool
samePasswordValidator x = password x == repeatPassword x
