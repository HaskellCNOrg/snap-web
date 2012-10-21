{-# LANGUAGE OverloadedStrings #-}
module Views.UserForm where

import           Control.Applicative    ((<$>), (<*>))
import           Data.Text              (Text)
import           Data.Maybe (fromMaybe)
import           Text.Digestive
import           Text.Digestive.FormExt

import           Models.User

-- |
data UserVo = UserVo
              { userEmail       :: Text
              , userDisplayName :: Text
              , userSite        :: Text
              }

------------------------------------------------------------------
--

signinForm :: Monad m => (Text,Text) -> Form Text m LoginUser
signinForm (a,b) = LoginUser
    <$> "loginName"      .: check a requiredValidator (text Nothing)
    <*> "password"       .: check b requiredValidator (text Nothing)
    <*> "repeatPassword" .: text Nothing

signupForm :: Monad m => (Text,Text) -> Form Text m LoginUser
signupForm (a,b) = check "Two Input password must be same" samePasswordValidator $
    LoginUser
    <$> "loginName"       .: (checkValidEmail . checkRequired a) (text Nothing)
    <*> "password"        .: checkRequired b (text Nothing)
    <*> "repeatPassword"  .: checkRequired "Please input the password again" (text Nothing)

samePasswordValidator :: LoginUser -> Bool
samePasswordValidator x = password x == repeatPassword x

------------------------------------------------------------------
--

-- | Prepare a form for display from a exists @User@.
--
userDetailForm :: Monad m => User -> Form Text m UserVo
userDetailForm u = UserVo
    <$> "userEmail"       .: text (Just $ _userEmail u)
    <*> "userDisplayName" .: text (Just $ _userDisplayName u)
    <*> "userSite"        .: text (Just $ fromMaybe "" $ _userSite u)

-- |
--
userForm :: Monad m => Form Text m UserVo
userForm  = UserVo
    <$> "userEmail"          .: text Nothing    -- update email is disallowed.
    <*> "userDisplayName"    .: text Nothing
    <*> "userSite"           .: text Nothing

------------------------------------------------------------------
