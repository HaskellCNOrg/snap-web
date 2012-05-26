{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Controllers.User 
       ( routes 
       , redirectToSignin 
       , withAuthUser ) where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.CatchIO (try)
import           Data.Text (Text)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Text.Digestive.Snap hiding (method)
import           Text.Digestive
import           Text.Templating.Heist
import qualified Data.ByteString as BS

import           Application
import           Controllers.Home
import           Models.Exception
import           Views.UserForm
import           Views.Utils
import           Views.UserSplices
import qualified Models.User as USER


routes :: [(BS.ByteString, Handler App App ())]
routes =  [ 
          ----
            ("/signup",  signup)
          , ("/signin",  signin)
          , ("/signout", method GET signout)
          ----
          , ("/user",    method GET viewUserH)
          , ("/userput/", method GET editUserH)
          , ("/userput/user", method POST saveUserH)
          ]

paramUserId :: BS.ByteString
paramUserId = "userid"

------------------------------------------------------------------------------

-- | Perform a action `AppHandler ()` within Auth user otherwise redirect to signin.
--
withAuthUser :: AppHandler () -> AppHandler ()
withAuthUser = requireUser appAuth redirectToSignin

-- | Redirect to signin page.
-- 
redirectToSignin :: AppHandler ()
redirectToSignin = redirect "/signin"

------------------------------------------------------------------------------
    
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
                        --result' <- try (with appAuth (USER.createNewUser u))
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
    errorMsg       <- (,) <$> lookupI18NValue "requiredLoginname" 
                          <*> lookupI18NValue "requiredPassword"
    (view, result) <- runForm "form" $ signinForm errorMsg
    case result of
        Just usr -> do
                  result' <- try (with appAuth $ USER.loginUser usr)
                  either (toPage . updateViewErrors view . showUE) toHome result'
        Nothing -> toPage view
    where toPage = renderDfPage "signin"
          toHome = const redirectToHome

          
------------------------------------------------------------------------------

-- | log out
-- 
signout :: AppHandler ()
signout = with appAuth logout >> redirectToHome


------------------------------------------------------------------------------

-- | Fetch @User@ details. As this handler used with @withAuthUser@, 
--   use `Just` for pattern matching.
-- 
viewUserH :: AppHandler ()
viewUserH = withAuthUser $ USER.findCurrentUser >>= toUserDetailPage
    

toUserDetailPage :: USER.User -> AppHandler ()
toUserDetailPage user = heistLocal (bindSplices (userDetailSplices user)) $ render "user-detail"

------------------------------------------------------------------------------

-- | Fetch @User@ details for editing.
-- 
editUserH :: AppHandler ()
editUserH = withAuthUser $ do
    user <- USER.findCurrentUser
    runForm "edit-user-form" (userDetailForm user) >>= (toTopicFormPage . fst)

toTopicFormPage :: View Text -> AppHandler ()
toTopicFormPage = renderDfPage "user-form"


--toEditTopicPageOr' =<< try (findOneTopic (read $ bsToS $ fromJust tid))

--toEditTopicPageOr' :: Either UserException Topic -> AppHandler ()    
--toEditTopicPageOr' = either (toTopicDetailPage . Left) toEditingPage
--    where toEditingPage t = runForm "edit-topic-form" (topicEditForm t) >>= (toTopicFormPage . fst)



-- | Persistence change made by user.
-- 
saveUserH :: AppHandler ()
saveUserH = withAuthUser $ do
    (view, result) <- runForm "edit-user-form" userForm
    case result of
      Just usrVo -> doUpdateUser usrVo
      Nothing  -> toTopicFormPage view  -- FIXME: bug..to form page should runForm first.
                                        -- however this branch wont be hit bacuse `saveTopicH` 
                                        -- only accept POST method

    where doUpdateUser vo = userVoToUser' vo >>= USER.saveUser >>= toUserDetailPage
          userVoToUser' :: UserVo -> AppHandler USER.User
          userVoToUser' vo = do
                            (Just authUser) <- with appAuth currentUser
                            return $ USER.User authUser (userEmail vo) (userDisplayName vo)
