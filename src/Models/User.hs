{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Models.User
    ( LoginUser(..)
    , User(..)
    , UserObjId
    , createNewUser
    , loginUser
    , resetPassword
    , findCurrentUser
    , findCurrentUserId
    , findOneUser
    , saveUser
    , hasUpdatePermission
    , isCurrentUserAdmin ) where

import           Control.Applicative                (pure, (<$>), (<*>))
import           Control.Monad
import           Control.Monad.CatchIO              (throw)
import           Control.Monad.State
import           Data.Baeson.Types
import           Data.Bson
import           Data.Function
import           Data.Maybe
import qualified Data.Text                          as T
import qualified Database.MongoDB                   as DB
import           Snap.Snaplet
import           Snap.Snaplet.Auth                  hiding (loginUser, saveUser)
import           Snap.Snaplet.Auth.Backends.MongoDB as SM
import           Snap.Snaplet.MongoDB

import           Application
import           Models.Exception
import           Models.Utils

-- | User Email plays another role as a unique loginName.
--
type Email = T.Text

type UserObjId = ObjectId

-- | A lightweight alternative to @AuthUser@. Use email prefix as login name.
--
data LoginUser = LoginUser
    { loginName      :: Email
    , password       :: T.Text
    , repeatPassword :: T.Text
    } deriving (Show)


-- | Extent to AuthUser in a separated Collection.
--   Its objectId is just same with objectId of AuthUser.
--
--   One reason is Snaplet-Auth is likely to do some modify, e.g. when login
--   which will fresh out our extension.
--
data User = User
    { _authUser        :: Maybe AuthUser  -- ^ Maybe type because dont need te fetch sometime.
    , _userEmail       :: Email           -- ^ TODO: deprecated as duplicated with AuthUSer.email
    , _userDisplayName :: T.Text
    , _userSite        :: Maybe T.Text    -- ^ User personal site.
    } deriving (Show)


-- | This is where User extra information saved, making diff wih AuthUser.
--
authUserCollection :: DB.Collection
authUserCollection = "users"

------------------------------------------------------------------------------

-- | Ask Snaplet-Auth to create AuthUser then extend it with additional fields.
--   Since there is no way to use @withBackend@ from Snaplet-Auth, I have to use
--   mongo.save to update the user collection.
--
createNewUser :: LoginUser -> AppHandler User
createNewUser lu = do
    authUser <- with appAuth $ createAuthUser' lu
    user <- saveUser $ User (Just authUser) (loginName lu) (extractUserName lu) (Just "")
    with appAuth $ loginUser lu
    return user
    where extractUserName = T.takeWhile (/= '@') . loginName


-- | Create a user leverage save function from snaplet-auth-mongo-backend,
--   without additional columns. This is not suppose to be used by upstream API.
--
--   Create user without activation approach thus login automatically.
--   Maybe use userLockedOutUntil when like to use mail activation.
--
createAuthUser' :: LoginUser -> Handler b (AuthManager b) AuthUser
createAuthUser' usr = do
    --mp <- gets minPasswdLen
    --when (passLength usr < mp) (throw $ PasswordTooShort mp)
    exists <- usernameExists (loginName usr)
    when exists (throw UserAlreadyExists)
    result <- createUser (loginName usr) (password' usr)
    case result of
      Left l -> throw $ UserException $ show l
      Right r -> return r
  where passLength = T.length . password
        password'  = textToBS . password

-- http://hpaste.org/69009, failure piece of code about `withBackend`.


------------------------------------------------------------------------------
resetPassword' :: IAuthBackend r
                  => LoginUser
                  -> r
                  -> IO AuthUser
resetPassword' (LoginUser login passwd _) backend = do
  result <- lookupByLogin backend login
  case result of
    Nothing -> throwUE UserNotFound
    Just u' -> do
      authuser <- setPassword u' $ textToBS passwd
      saved <- save backend authuser
      either throwUE return saved

resetPassword :: LoginUser -> AppHandler AuthUser
resetPassword user = with appAuth $ withBackend $ \r -> liftIO (resetPassword' user r)

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


-- | Find One User without initlize its _authUser field. Usually for user basic information.
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

-- | Transform @User@ to mongoDB document.
--   Assume "_id" already exists because AuthUser should be created before.
--
userToDocument :: User -> Document
userToDocument user =  [ "_id"    .= userId (fromJust $ _authUser user)
                        , "email" .= _userEmail user
                        , "display_name"  .= _userDisplayName user
                        , "url"   .= _userSite user
                        ]


-- | Kind of ORM.
--
userToTopic :: Maybe AuthUser -> Document -> Parser User
userToTopic au d = User
                   <$> pure au
                   <*> d .: "email"
                   <*> d .: "display_name"
                   <*> d .:? "url"


userFromDocumentOrThrow :: (Document -> Parser User) -> Document -> IO User
userFromDocumentOrThrow f d = case parseEither f d of
    Left e  -> throw $ UserException e
    Right r -> return r


------------------------------------------------------------------------------

getUserId :: AuthUser -> Maybe ObjectId
getUserId = fmap SM.userIdToObjectId . userId

-- | Basically email of user is a unique because it is used as loginName of AuthUser.
--   In additions, some @User@ doesn't get its @_authUser@ populated.
--
userEq :: User -> User -> Bool
userEq = (==) `on` _userEmail

----------------------------------------------------------- ROLE & PREMISSIONS

-- | Get admin roles from config file.
--
getAdminRole :: AppHandler Role
getAdminRole = gets _adminRole

-- | Is the @User@ an admin user. a.k.a has Admin role.
--
isUserAdmin :: AuthUser -> AppHandler Bool
isUserAdmin user = liftM (hasRole user) getAdminRole

-- | Whether current login user an admin user. a.k.a has Admin role.
--
isCurrentUserAdmin :: AppHandler Bool
isCurrentUserAdmin = findCurrentAuthUser >>= isUserAdmin

-- | Whether @User@ has a particular @Role@
--
hasRole :: AuthUser -> Role -> Bool
hasRole au r = r `elem` userRoles au

-- | Admin and Author-self has edit/delete permission.
--
hasUpdatePermission :: User   -- ^ Author of some.
                    -> AppHandler Bool
hasUpdatePermission author = (||)
                             <$> liftM (userEq author) findCurrentUser
                             <*> isCurrentUserAdmin
