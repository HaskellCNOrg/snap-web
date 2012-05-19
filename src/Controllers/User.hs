{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Controllers.User where

------------------------------------------------------------------------------

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Monad.Trans
import           Text.Templating.Heist
import           Control.Monad.CatchIO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import           Data.Typeable

import           Application
import           Controllers.Utils
import           Controllers.Home
import           Views.UserForm



------------------------------------------------------------------------------

-- TODO
-- 2. why browser ask for remember password when signup but not signin.

-- 3. auto login after signup successfully.

    
-- | sign up user and redirect to home page.
--   Snap `registerUser` allow empty username and password
-- 
signup :: AppHandler ()
signup = do
          errorMsg       <- (,) <$> lookupI18NValue "requiredLoginname" 
                                <*> lookupI18NValue "requiredPassword"
          (view, result) <- runForm "form" $ signupForm errorMsg
          case result of
              Just u -> do
                        exitst <- with appAuth (usernameExists (loginName u))
                        if exitst then
                            renderToSignupPage $ upadteErrors view $ BackendError "UserExists"
                            else (do
                                     result <- try (with appAuth (createUser (loginName u) (password' u)))
                                     either (renderToSignupPage . upadteErrors view) toHome result )
              Nothing -> renderToSignupPage view
          where toHome  x                 = with appAuth loginByRememberToken >> redirectToHome
                upadteErrors              :: View T.Text -> BackendError -> View T.Text
                upadteErrors v e          = v { viewErrors = viewErrors v ++ [([], T.pack $ show e)]}
                password'                 = textToBs . password
                renderToSignupPage v      = heistLocal (bindDigestiveSplices v) $ render "signup"

------------------------------------------------------------------------------

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
    (view, result) <- runForm "form" $ signinForm errorMsg
    case result of
        Just form -> do
                  authResult <- with appAuth $ loginUser' form
                  either (renderToSigninPage . upadteErrors view) toHome authResult
        Nothing -> renderToSigninPage view
    where loginUser' x              = loginByUsername (username' x) (password' x) True
          username' = textToBs . loginName
          password' = ClearText . textToBs . password
          renderToSigninPage v      = heistLocal (bindDigestiveSplices v) $ render "signin"
          toHome x                  = redirectToHome

          ----------------------------FIXME: user friendly error message per errors.
          upadteErrors v e          = v { viewErrors = viewErrors v ++ [([], T.pack $ show e)]}
          

------------------------------------------------------------------------------

-- | log out
-- 
signoutG :: AppHandler ()
signoutG = with appAuth logout >> redirect "/"
