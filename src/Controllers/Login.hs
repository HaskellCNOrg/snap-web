{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Login where

import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist

import           Application
import           Controllers.Utils

------------------------------------------------------------------------------
-- TODO
-- 1. catch error for auth snaplet.
-- 2. FIXME: required field validation. e.g. password required.
--    display user-friendly message.


-- | Show sign up page
-- 
signupG :: AppHandler ()
signupG = render "signup"
    
-- | sign up user and redirect to home page
-- 
--   
signupP :: AppHandler ()
signupP = do
    with appAuth $ registerUser "username" "password"
    redirect "/"

------------------------------------------------------------------------------
--
-- TODO
-- 1. catch error for auth snaplet.
-- 2. FIXME: ERROR Handler, e.g. user doesnot exists, password incorrect
--   FIXME: use `loginUser` function


signinG :: AppHandler ()
signinG = do
    heistLocal (bindString "test" "Login") $ render "signin"

signinP :: AppHandler ()
signinP = do
    userName <- decodedParam "username"
    password <- decodedParam "password"
    with appAuth $ loginByUsername userName (ClearText password) True
    redirect "/"

------------------------------------------------------------------------------

-- | log out
-- 
signoutG :: AppHandler ()
signoutG = with appAuth $ logoutUser (redirect "/")

