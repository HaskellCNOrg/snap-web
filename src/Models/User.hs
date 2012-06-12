{-# LANGUAGE OverloadedStrings #-}

module Models.User 
    ( LoginUser(..)
    , User(..)
    , UserObjId
    , createNewUser
    , loginUser
    , findCurrentUser
    , findCurrentUserId
    , findCurrentAuthUser
    , findOneUser
    , saveUser
    , adminRoles
    , isCurrentUserAdmin ) where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad
import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Data.Baeson.Types
import           Data.Bson
import           Data.Maybe
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (loginUser, saveUser)
import           Snap.Snaplet.Auth.Backends.MongoDB as SM
import           Snap.Snaplet.MongoDB
import qualified Data.Text as T
import qualified Database.MongoDB as DB
import Snap.Snaplet.Environments


import           Models.Exception
import           Models.Utils
import           Application

type Email = T.Text
type UserObjId = ObjectId

-- | A lightweight alternative to @AuthUser@. Use email prefix as login name.
-- 
data LoginUser = LoginUser
    { loginName :: Email
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
    { _authUser    :: Maybe AuthUser  -- ^ Maybe type because dont need te fetch sometime.
    , _userEmail   :: Email
    , _userDisplayName :: T.Text  
    , _userSite    :: T.Text  -- ^ User personal site.
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
    saveUser $ User (Just authUser) (loginName lu) (extractUserName lu) ""
    where extractUserName = T.takeWhile (/= '@') . loginName


-- | Create a user leverage save function from snaplet-auth-mongo-backend,
--   without additional columns. This is not suppose to be used by upstream API.
-- 
--   Create user without activation approach thus login automatically.
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
loginUser lu = do
              res <- loginByUsername (username' lu) (password' lu) True
              either throwUE return res
              where username' = textToBS . loginName
                    password' = ClearText . textToBS . password


------------------------------------------------------------------------------

-- | NOTES: Assume those handlers will be used after login. a.k.a will be used in @withAuthUser@.
--   Maybe improve in future.
-- 
findCurrentUser :: AppHandler User
findCurrentUser = do 
                    authUser <- findCurrentAuthUser
                    res <- findUser' authUser
                    either failureToUE (userFromDoc authUser) res
                  where findUser' = withFindUser' . oid
                        oid       = SM.userIdToObjectId . fromJust . userId
                        userFromDoc au = liftIO . userFromDocumentOrThrow (userToTopic $ Just au)

findCurrentAuthUser :: AppHandler AuthUser
findCurrentAuthUser = liftM fromJust (with appAuth currentUser)

findCurrentUserId :: AppHandler (Maybe ObjectId)
findCurrentUserId = liftM (join . fmap getUserId) (with appAuth currentUser)


-- | Find One User
-- 
findOneUser :: ObjectId -> AppHandler User
findOneUser oid = do
    res <- withFindUser' oid
    either failureToUE (liftIO . userFromDocumentOrThrow (userToTopic Nothing)) res

withFindUser' oid = eitherWithDB $ DB.fetch (DB.select [ "_id" =: oid ] authUserCollection)

------------------------------------------------------------------------------

-- | Save modification to @User@ Collection. @AuthUser@ is not likely to be update by user.
-- 
saveUser :: User -> AppHandler User
saveUser lu = do
             res <- eitherWithDB $ DB.save authUserCollection $ userToDocument lu
             either failureToUE (const $ return lu) res 


------------------------------------------------------------------------------

-- | Get admin roles from config file.
-- 
adminRoles :: AppHandler (Maybe Role)
adminRoles = do
    role <- lookupConfig "auth.admin-roles"
    case role of
      Nothing -> return Nothing
      Just bs -> return $ Just (Role bs)


isCurrentUserAdmin :: AppHandler Bool
isCurrentUserAdmin = do
    role <- adminRoles
    authUser <- findCurrentAuthUser
    case role of
      Nothing -> return False
      Just r  -> return $ r `elem` userRoles authUser


------------------------------------------------------------------------------

-- | Transform @User@ to mongoDB document.
--   Assume "_id" already exists because AuthUser should be created before.
-- 
userToDocument :: User -> Document
userToDocument user =  [ "_id"          .= userId (fromJust $ _authUser user)
                        , "userEmail"   .= _userEmail user
                        , "displayName" .= _userDisplayName user
                        , "userSite"    .= _userSite user
                        ]


-- | Kind of ORM.
-- 
userToTopic :: Maybe AuthUser -> Document -> Parser User
userToTopic au d = User 
                   <$> pure au
                   <*> d .: "userEmail"
                   <*> d .: "displayName"
                   <*> d .: "userSite"


userFromDocumentOrThrow :: (Document -> Parser User) -> Document -> IO User
userFromDocumentOrThrow f d = case parseEither f d of
    Left e  -> throw $ BackendError $ show e
    Right r -> return r


------------------------------------------------------------------------------

getUserId :: AuthUser -> Maybe ObjectId
getUserId = fmap SM.userIdToObjectId . userId

--getUserDisplayName :: ObjectId -> AppHandler T.Text
--getUserDisplayName = liftM _userDisplayName . findOneUser
