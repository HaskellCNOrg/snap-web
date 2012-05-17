{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.User where

------------------------------------------------------------------------------

import Control.Applicative ((<$>), (<*>))
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
--import           Control.Monad.Trans
import           Text.Templating.Heist
import qualified Data.Text as T

import           Application
import           Controllers.Utils
import           Controllers.Home
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
signupP = with appAuth (registerUser "username" "password") >> redirectToHome

------------------------------------------------------------------------------
--
-- TODO
-- 1. [ ] catch error for auth snaplet.
-- 2. [ ] FIXME: ERROR Handler, e.g. user doesnot exists, password incorrect
-- 3. [ ] FIXME: use `loginUser` function
-- 

-- | Sign in the user.
-- 
signin :: AppHandler ()
signin = do
    -- 
    -- 1. fetch i18n message for error
    -- 2. run digestive functor form
    -- 3. return to signin page if form validation failed.
    -- 4. otherwise doing authentication
    -- 5. return to sign page if any errors 
    -- 6. otherwise return to home page.
    -- 
    errorMsg       <- (,) <$> lookupI18NValue "requiredLoginname" 
                          <*> lookupI18NValue "requiredPassword"
    (view, result) <- runForm "form" $ userForm errorMsg
    case result of
        Just form -> do
                  authResult <- with appAuth $ loginUser' form
                  either (renderToSigninPage . upadteErrors view) toHome authResult
        Nothing -> renderToSigninPage view
    where loginUser' x              = loginByUsername (username' x) (password' x) True
          username' (LoginUser u _) = textToBs u
          password' (LoginUser _ p) = ClearText $ textToBs p
          -- FIXME: user friendly error message per errors.
          upadteErrors v e          = v { viewErrors = viewErrors v ++ [([], T.pack $ show e)]}
          renderToSigninPage v      = heistLocal (bindDigestiveSplices v) $ render "signin"
          toHome x                  = redirectToHome

------------------------------------------------------------------------------

-- | log out
-- 
signoutG :: AppHandler ()
signoutG = with appAuth logout >> redirect "/"
