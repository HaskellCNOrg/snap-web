{-# LANGUAGE OverloadedStrings #-}

module Models.User 
    ( LoginUser(..)
    , User(..)
    , createNewUser
    , loginUser
    , findCurrentUser
    , saveUser ) where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad
import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Data.Baeson.Types
import           Data.Bson
import           Data.Maybe
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (loginUser, saveUser)
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.Auth.Backends.MongoDB as SM
import qualified Data.Text as T
import qualified Database.MongoDB as DB

import           Models.Exception
import           Models.Utils
import           Application

-- | A lightweight alternative to @AuthUser@
-- 
data LoginUser = LoginUser
    { loginName :: T.Text
    , password  :: T.Text
    , repeatPassword :: T.Text
    } deriving (Show)


-- | Extent to AuthUser in a separated Collection.
--   Its objectId is just same with objectId of AuthUser.
-- 
--   One reason is Snaplet-Auth is likely to do some modify, e.g. when login
--   which will fresh out our extension.
-- 
data User = User 
    { _authUser :: AuthUser
    , _userEmail :: T.Text 
    , _displayName :: T.Text 
    } deriving (Show)

-- | FIXME: this is duplicated defination with what in mongoDBBackend.
--          Need to figure out the root cause why mongoBackend failed to 
--          read a snaplet.cfg file. Hence, could reduce this duplication.
-- 
authUserCollection :: DB.Collection
authUserCollection = u "users"

------------------------------------------------------------------------------

-- | Ask Snaplet-Auth to create AuthUser then extend it with additional fields.
--   Since there is no way to use @withBackend@ from Snaplet-Auth, I have to use
--   mongo.save to update the user collection.
-- 
createNewUser :: LoginUser -> AppHandler User
createNewUser lu = do
    authUser <- with appAuth $ createAuthUser' lu
    -- FIXME: do not hard code...
    saveUser $ User authUser "test@test.com" "test"


-- | Create a user leverage save function from snaplet-auth-mongo-backend,
--   without additional columns. This is not suppose to be used by upstream API.
-- 
--   Create user without activation appoach thus login automatically.
--   Maybe use userLockedOutUntil when like to use mail activation.
--
createAuthUser' :: LoginUser -> Handler b (AuthManager b) AuthUser
createAuthUser' usr = do
    mp <- gets minPasswdLen
    when (passLength usr < mp) (throw $ PasswordTooShort mp)
    exists <- usernameExists (loginName usr)
    when exists (throw UserAlreadyExists)
    authUsr <- createUser (loginName usr) (password' usr)
    forceLogin authUsr >>= either throwUE return
  where passLength    = T.length . password
        password'     = textToBS . password

-- http://hpaste.org/69009, failure piece of code about `withBackend`.


------------------------------------------------------------------------------

loginUser :: LoginUser -> Handler b (AuthManager b) AuthUser
loginUser u = do
              res <- loginByUsername (username' u) (password' u) True
              either throwUE return res
              where username' = textToBS . loginName
                    password' = ClearText . textToBS . password


------------------------------------------------------------------------------

-- | NOTES: Assume this handler will be used after login. a.k.a will be used in @withAuthUser@.
--   Maybe improve in future.
-- 
findCurrentUser :: AppHandler User
findCurrentUser = do 
                    (Just authUser) <- with appAuth currentUser
                    res <- findOneUser authUser
                    either failureToUE (liftIO . userFromDocumentOrThrow (userToTopic authUser)) res
                  where findOneUser u = eitherWithDB $ DB.fetch (DB.select [ "_id" =: oid u ] authUserCollection)
                        oid           = SM.userIdToObjectId . fromJust . userId


------------------------------------------------------------------------------

-- | Save modification to @User@ Collection. @AuthUser@ is not likely to be update by user.
-- 
saveUser :: User -> AppHandler User
saveUser u = do
             res <- eitherWithDB $ DB.save authUserCollection $ userToDocument u
             either failureToUE (const $ return u) res 


------------------------------------------------------------------------------

-- | Transform @User@ to mongoDB document.
--   Nothing of id mean new topic thus empty "_id" let mongoDB generate objectId.
-- 
userToDocument :: User -> Document
userToDocument user =  [ "_id"          .= userId (_authUser user)
                        , "userEmail"   .= _userEmail user
                        , "displayName" .= _displayName user
                        ]


-- | Kind of ORM.
-- 
userToTopic :: AuthUser -> Document -> Parser User
userToTopic au d = User 
                   <$> pure au
                   <*> d .: "userEmail"
                   <*> d .: "displayName"


userFromDocumentOrThrow :: (Document -> Parser User) -> Document -> IO User
userFromDocumentOrThrow f d = case parseEither f d of
    Left e  -> throw $ BackendError $ show e
    Right r -> return r


------------------------------------------------------------------------------

--getUserIdText :: User -> Maybe T.Text
--getUserIdText = fmap unUid . userId . _authUser

