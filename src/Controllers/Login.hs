{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Login where

import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import           Text.Digestive.Heist
import           Text.Digestive.Snap

import           Application
import           Controllers.Utils
import           Views.UserForm
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
-- 1. [ ] catch error for auth snaplet.
-- 2. [ ] FIXME: ERROR Handler, e.g. user doesnot exists, password incorrect
-- 3. [ ] FIXME: use `loginUser` function
-- 4. [ ] Question: since GET / POST goes to this single point, 
--             will runForm check for secure when GET the result should be Nothing ???
-- 
signin :: AppHandler ()
signin = do
    (view, result) <- runForm "form" userForm
    case result of
        Just x -> (with appAuth $ loginByUsername (username' x) (password' x) True) >> redirect "/"
        Nothing -> heistLocal (bindDigestiveSplices view) $ render "signin"
    where username' (LoginUser u _) = textToBs u
          password' (LoginUser _ p) = ClearText $ textToBs p

------------------------------------------------------------------------------

-- | log out
-- 
signoutG :: AppHandler ()
signoutG = with appAuth $ logoutUser (redirect "/")
