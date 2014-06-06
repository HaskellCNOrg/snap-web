{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.User
       ( routes
       , redirectToSignin
       , withAuthUser ) where

------------------------------------------------------------------------------
import           Control.Monad.CatchIO (try)
import qualified Data.ByteString       as BS
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Heist.Interpreted     as I
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive
import           Text.Digestive.Snap   hiding (method)

import           Application
import           Controllers.Exception (toErrorPage)
import           Controllers.Home
import           Models.Exception
import qualified Models.User           as USER
import           Models.Utils
import           Views.UserForm
import           Views.UserSplices
import           Views.SharedSplices
import           Views.Utils


routes :: [(BS.ByteString, Handler App App ())]
routes =  [
          ----
            ("/signup",  signup)
          , ("/signin",  signin)
          , ("/signout", method GET signout)
          , ("/forgotPassword", forgotPassword)
          ----
          , ("/user",      method GET viewUserH)
          , ("/user/:uid", method GET viewUserH)
          , ("/userput/:uid", method GET editUserH)
          , ("/userput", method POST saveUserH)
          ]

uidP :: BS.ByteString
uidP = "uid"

redirectToUserDetailPage :: AppHandler ()
redirectToUserDetailPage = redirectToUserDetailPage' ""

redirectToUserDetailPage' :: BS.ByteString -> AppHandler ()
redirectToUserDetailPage' = redirect303 . BS.append "/user/"

------------------------------------------------------------------------------

-- | Perform a action `AppHandler ()` within Auth user otherwise redirect to signin.
--   FIXME: 1. Waring message
--          2. Maybe redirect back to last page but home page.
--          3. Another improved handler with premission check.
--
withAuthUser :: AppHandler () -> AppHandler ()
withAuthUser = requireUser appAuth redirectToSignin

-- | Redirect to signin page.
--
redirectToSignin :: AppHandler ()
redirectToSignin = do
  v <- genNextPageParam
  redirect303 $ BS.append "/signin" v


------------------------------------------------------------------------------

-- | sign up user and redirect to home page.
--   Snap `registerUser` allow empty username and password
--
signup :: AppHandler ()
signup = do
          (view, result) <- runForm "form" signupForm
          case result of
              Just u -> do
                        result' <- try (USER.createNewUser u)
                        either (toPage . updateViewErrors view . showUE) toHome result'
              Nothing -> toPage view
          where toHome = const redirectToHome
                toPage = renderDfPage "signup"


------------------------------------------------------------------------------

-- | Sign in the user.
--
-- 1. fetch i18n message for error
-- 2. run digestive functor form
-- 3. return to signin page if form validation failed.
-- 4. otherwise doing authentication
-- 5. return to sign page if any errors
-- 6. otherwise return to home page.
--
signin :: AppHandler ()
signin = do
    nextPageURI <- decodedParamMaybe nextPageParam
    (view, result) <- runForm "form" (signinForm $ fmap bsToText nextPageURI)
    case result of
        Just usr -> do
                  result' <- try (with appAuth $ USER.loginUser $ loginFormUserToUser' usr)
                  either (toPage . updateViewErrors view . showUE) (toHome $ _nextPageUri usr) result'
        Nothing -> toPage view
    where toPage = renderDfPage "signin"
          -- | TODO: validate URIs
          toHome uri
            | T.null uri = const redirectToHome
            | otherwise = const (redirect $ textToBS uri)

loginFormUserToUser' :: LoginFormUser -> USER.LoginUser
loginFormUserToUser' (LoginFormUser n p _) = USER.LoginUser n p ""


------------------------------------------------------------------------------


-- | Reset password
-- MAYBE: improve it to leverage setPasswordResetToken from snap.
--
forgotPassword :: AppHandler ()
forgotPassword = do
  (view, result) <- runForm "form" resetPasswordForm
  case result of
    Nothing -> toPage view
    Just u -> do
      result' <- try (USER.resetPassword u)
      either (toPage . updateViewErrors view . showUE)
              toLoginPage
              result'
  where toPage = renderDfPage "forgot-password"
        -- TODO: show successful message.
        toLoginPage = const $ redirect "/signin"


------------------------------------------------------------------------------


-- | log out
--
signout :: AppHandler ()
signout = with appAuth logout >> redirectToHome


------------------------------------------------------------------------------

-- | Fetch @User@ details. As this handler used with @withAuthUser@.
--
viewUserH :: AppHandler ()
viewUserH = withAuthUser $ decodedParamTextMaybe uidP
            >>= try . findUserOrCurrent
            >>= toUserDetailPage

findUserOrCurrent :: Maybe Text -> AppHandler USER.User
findUserOrCurrent = maybe USER.findCurrentUser (USER.findOneUser . textToObjectId)


toUserDetailPage :: Either UserException USER.User -> AppHandler ()
toUserDetailPage user = heistLocal (I.bindSplices (userDetailSplices user)) $ render "user-detail"


------------------------------------------------------------------------------

-- | Fetch @User@ details for editing.
--
editUserH :: AppHandler ()
editUserH = withAuthUser $ do
    uid <- decodedParamTextMaybe uidP
    user <- try (findUserOrCurrent uid)
    either (const $ toUserDetailPage user) runFormToFormPage' user
    where runFormToFormPage' u = runForm "edit-user-form" (userDetailForm u)
                                 >>= (toUserFormPage . fst)


toUserFormPage :: View Text -> AppHandler ()
toUserFormPage = renderDfPage "user-form"


-- | Persistence change made by user.
--
saveUserH :: AppHandler ()
saveUserH = withAuthUser $ do
    (_, result) <- runForm "edit-user-form" userForm
    case result of
      Just usrVo -> doUpdateUser usrVo
      Nothing    -> editUserH  -- TODO:This is silence failue to detail view page of current user.
    where doUpdateUser vo    = userVoToUser' vo >>= (try . USER.saveUser) >>= eitherSuccess'
          eitherSuccess' res = case res of
                                 Left _  -> toUserDetailPage res
                                 Right u  -> redirectToUserDetailPage' (uidBS u)
          uidBS = textToBS . USER.getUserIdText

-- | Transform @UserVo@ to @User@.
--   Update email is not allowed thus just get from authUser
--   because it is designed that loginName is email address.
--
userVoToUser' :: UserVo -> AppHandler USER.User
userVoToUser' (UserVo vid _ vname vsite) = do
                  user <- USER.findOneUser $ textToObjectId vid
                  return user { USER._userDisplayName = vname
                              , USER._userSite = Just vsite
                              }
------------------------------------------------------------------------------
