{-# LANGUAGE OverloadedStrings #-}

module Models.User 
    ( LoginUser(..)
    , User(..)
    , createNewUser
    , loginUser
    , findCurrentUser ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Data.Baeson.Types
import           Data.Bson
import           Data.Maybe
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (loginUser)
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.Auth.Backends.MongoDB as SM
import qualified Data.Text as T
import qualified Database.MongoDB as DB

import           Models.Exception
import           Models.Utils
import           Application

-- | FIXME: Rename to LoginUserVo
-- 
data LoginUser = LoginUser
    { loginName :: T.Text
    , password  :: T.Text
    , repeatPassword :: T.Text
    } deriving (Show)

-- | Extent to AuthUser
-- 
data User = User 
    { _authUser :: AuthUser 
    , _userEmail :: T.Text 
    , _displayName :: T.Text 
    }

-- | FIXME: this is duplicated defination with what in mongoDBBackend.
--          Need to figure out the root cause why mongoBackend failed to 
--          read a snaplet.cfg file. Hence, could reduce this duplication.
-- 
authUserCollection :: DB.Collection
authUserCollection = u "auth_user"

------------------------------------------------------------------------------

-- | Ask Snaplet-Auth to create AuthUser then extend it with additional fields.
--   Since there is no way to use @withBackend@ from Snaplet-Auth, I have to use
--   mongo.save to update the user collection.
-- 
createNewUser :: LoginUser -> AppHandler User
createNewUser lu = do
    authUser <- with appAuth $ createNewUser' lu
    -- FIXME: do not hard code...
    let user' = User authUser  "test@test.com" "test"
    res <- saveUserExtension user'
    either throwUE (const $ return user') res
  where saveUserExtension u = eitherWithDB $ DB.save authUserCollection $ userToDocument u


-- | Create a user leverage save function from snaplet-auth-mongo-backend,
--   without additional columns. This is not suppose to be used by upstream API.
-- 
--   Create user without activation appoach thus login automatically.
--   Maybe use userLockedOutUntil when like to use mail activation.
--
createNewUser' :: LoginUser -> Handler b (AuthManager b) AuthUser
createNewUser' usr = do
    mp <- gets minPasswdLen
    when (passLength usr < mp) (throw $ PasswordTooShort mp)
    exists <- usernameExists (loginName usr)
    when exists (throw UserAlreadyExists)
    authUsr <- createUser (loginName usr) (password' usr)
    forceLogin authUsr >>= either throwUE return
  where passLength    = T.length . password
        password'     = textToBS . password

-- http://hpaste.org/69009


------------------------------------------------------------------------------

loginUser :: LoginUser -> Handler b (AuthManager b) AuthUser
loginUser u = do
              res <- loginByUsername (username' u) (password' u) True
              either throwUE return res
              where username' = textToBS . loginName
                    password' = ClearText . textToBS . password


------------------------------------------------------------------------------

-- | Assume this handler will be used after login. a.k.a will be used in @withAuthUser@.
--   Maybe improve in future.
-- 
findCurrentUser :: AppHandler User
findCurrentUser = do 
                    (Just user) <- with appAuth currentUser
                    res <- findOneUser user
                    either failureToUE (liftIO . userFromDocumentOrThrow) res
                  where findOneUser u = eitherWithDB $ DB.fetch (DB.select [ "_id" =: oid u ] authUserCollection)
                        oid = SM.userIdToObjectId . fromJust . userId 

------------------------------------------------------------------------------

-- | Transform @User@ to mongoDB document.
--   Nothing of id mean new topic thus empty "_id" let mongoDB generate objectId.
-- 
userToDocument :: User -> Document
userToDocument user = SM.usrToMong (_authUser user)
                      ++ [  "userEmail"   .= _userEmail user
                          , "displayName"  .= _displayName user
                          ]

userToTopic :: Document -> Parser User
userToTopic d = makeUsr (SM.usrFromMong d)
                    where makeUsr au = User 
                                       <$> au
                                       <*> d .: "userEmail"
                                       <*> d .: "displayName"

userFromDocumentOrThrow :: Document -> IO User
userFromDocumentOrThrow d = case parseEither userToTopic d of
    Left e  -> throw $ BackendError $ show e
    Right r -> return r

------------------------------------------------------------------------------
