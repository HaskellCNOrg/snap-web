{-# LANGUAGE OverloadedStrings #-}
module Views.UserForm where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Text.Digestive


import Views.Validators
import Models.User

-- | 
data UserVo = UserVo
              { userEmail        :: Text
              , userDisplayName  :: Text
              }

------------------------------------------------------------------
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

------------------------------------------------------------------
--

userDetailForm :: Monad m => User -> Form Text m UserVo
userDetailForm u = UserVo
    <$> "userEmail"          .: check "email is required." requiredValidator (text $ Just $ _userEmail u)
    <*> "userDisplayName"    .: text (Just $ _displayName u)

userForm :: Monad m => Form Text m UserVo
userForm  = UserVo
    <$> "userEmail"          .: check "email is required." requiredValidator (text Nothing)
    <*> "userDisplayName"    .: text Nothing

------------------------------------------------------------------
