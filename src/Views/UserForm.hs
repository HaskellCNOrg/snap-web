{-# LANGUAGE OverloadedStrings #-}
module Views.UserForm where

import           Control.Applicative    ((<$>), (<*>))
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Text.Digestive
import           Text.Digestive.FormExt

import           Models.User
import           Models.Utils

-- |
data UserVo = UserVo
              { userVoId        :: Text
              , userEmail       :: Text
              , userDisplayName :: Text
              , userSite        :: Text
              } deriving (Show)

------------------------------------------------------------------
--
signinForm :: Monad m => Form Text m LoginUser
signinForm = LoginUser
    <$> "loginName"      .: checkRequired loginNameRequired (text Nothing)
    <*> "password"       .: checkRequired passwordRequired (text Nothing)
    <*> "repeatPassword" .: text Nothing

signupForm :: Monad m => Form Text m LoginUser
signupForm = check "Two Input password must be same" samePasswordValidator $
    LoginUser
    <$> "loginName"       .: (checkValidEmail . checkRequired loginNameRequired) (text Nothing)
    <*> "password"        .: passwordValidator (text Nothing)
    <*> "repeatPassword"  .: checkRequired "Please input the password again" (text Nothing)

samePasswordValidator :: LoginUser -> Bool
samePasswordValidator x = password x == repeatPassword x

passwordValidator :: Monad m => Form Text m Text -> Form Text m Text
passwordValidator = checkMaxLengthWith 20 "Password"
                    . checkMinLengthWith 8 "Password"
                    . checkRequired passwordRequired

resetPasswordForm :: Monad m => Form Text m LoginUser
resetPasswordForm = signupForm

loginNameRequired, passwordRequired :: Text
loginNameRequired = "Login Name is required"
passwordRequired = "Password is required"


------------------------------------------------------------------
--

-- | Prepare a form for display from a exists @User@.
--
userDetailForm :: Monad m => User -> Form Text m UserVo
userDetailForm u = UserVo
    <$> "userVoId"       .: text (Just $ objectIdToText $ getUserId' u)
    <*> "userEmail"       .: text (Just $ _userEmail u)
    <*> "userDisplayName" .: text (Just $ _userDisplayName u)
    <*> "userSite"        .: text (Just $ fromMaybe "" $ _userSite u)

-- |
--
userForm :: Monad m => Form Text m UserVo
userForm  = UserVo
    <$> "userVoId"        .: text Nothing
    <*> "userEmail"       .: text Nothing
    <*> "userDisplayName" .: text Nothing
    <*> "userSite"        .: text Nothing

------------------------------------------------------------------
