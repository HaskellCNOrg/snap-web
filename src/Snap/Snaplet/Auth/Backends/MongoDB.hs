{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-
THIS WORK IS COPY FROM: https://gist.github.com/2347061
-}

module Snap.Snaplet.Auth.Backends.MongoDB
  ( initMongoAuth
  ) where

------------------------------------------------------------------------------
import           Control.Arrow
import           Data.Aeson
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Maybe
import           Data.Pool
import           Database.MongoDB (Document, Val(..), u, Field((:=)))
import           Database.MongoDB as M
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Common
import           System.IO.Pool (Pool, Factory (Factory))
import           Web.ClientSession
import           Snap.Snaplet.MongoDB

data MongoAuthManager = MongoAuthManager
    { mongoTable    :: String
    , mongoConnPool :: MongoDB
    }


------------------------------------------------------------------------------
-- | Simple function to get auth settings from a config file.  All options
-- are optional and default to what's in defAuthSettings if not supplied.
settingsFromConfig :: Initializer b (AuthManager b) AuthSettings
settingsFromConfig = do
    config <- getSnapletUserConfig
    minPasswordLen <- liftIO $ C.lookup config "minPasswordLen"
    let pw = maybe id (\x s -> s { asMinPasswdLen = x }) minPasswordLen
    rememberCookie <- liftIO $ C.lookup config "rememberCookie"
    let rc = maybe id (\x s -> s { asRememberCookieName = x }) rememberCookie
    rememberPeriod <- liftIO $ C.lookup config "rememberPeriod"
    let rp = maybe id (\x s -> s { asRememberPeriod = Just x }) rememberPeriod
    lockout <- liftIO $ C.lookup config "lockout"
    let lo = maybe id (\x s -> s { asLockout = Just (second fromInteger x) })
                   lockout
    siteKey <- liftIO $ C.lookup config "siteKey"
    let sk = maybe id (\x s -> s { asSiteKey = x }) siteKey
    return $ (pw . rc . rp . lo . sk) defAuthSettings


------------------------------------------------------------------------------
-- | Initializer for the MongoDB backend to the auth snaplet.
--
initMongoAuth
  :: Lens b (Snaplet SessionManager)  -- ^ Lens to the session snaplet
  -> Snaplet MongoDB  -- ^ The mongodb snaplet
  -> SnapletInit b (AuthManager b)
initMongoAuth sess db = makeSnaplet "mongodb-auth" desc datadir $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- settingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let manager = MongoAuthManager authTable $
                                      pgPool $ getL snapletValue db
    liftIO $ createTableIfMissing manager
    rng <- liftIO mkRNG
    return $ AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "A MongoDB backend for user authentication"
    datadir = Just $ liftM (++"/resources/auth") getDataDir


instance IAuthBackend MongoAuthManager where
    save MongoAuthManager{..} u@AuthUser{..} = do


    lookupByUserId MongoAuthManager{..} uid = do

    lookupByLogin MongoAuthManager{..} login = do

    lookupByRememberToken MongoAuthManager{..} token = do

    destroy MongoAuthManager{..} AuthUser{..} = do

userToDoc AuthUser{..} =
    [ "_id"                  =: userId
    , "login"                =: userLogin
    , "password"             =: userPassword
    , "activatedAt"          =: userActivatedAt
    , "suspendedAt"          =: usersuspendedAt
    , "rememberToken"        =: userRememberToken
    , "loginCount"           =: userLoginCount
    , "userFailedLoginCount" =: userFailedLoginCount
    , "lockedOutUntil"       =: userLockedOutUntil
    , "currentLoginAt"       =: userCurrentLoginAt
    , "lastLoginAt"          =: userLastLoginAt
    , "currentLoginIp"       =: userCurrentLoginIp
    , "lastLoginIp"          =: userLastLoginIp
    , "createdAt"            =: userCreatedAt
    , "updatedAt"            =: userUpdatedAt
    , "roles"                =: userRoles
    ]

userFromDoc doc = AuthUser
    <$> T.pack . cast =<< M.look "_id" doc
    <*> cast =<< M.look "login" doc
    <*> cast =<< M.look "password" doc
    <*> cast =<< M.look "activatedAt" doc
    <*> cast =<< M.look "suspendedAt" doc
    <*> cast =<< M.look "rememberToken" doc
    <*> cast =<< M.look "loginCount" doc
    <*> cast =<< M.look "userFailedLoginCount" doc
    <*> cast =<< M.look "lockedOutUntil" doc
    <*> cast =<< M.look "currentLoginAt" doc
    <*> cast =<< M.look "lastLoginAt" doc
    <*> cast =<< M.look "currentLoginIp" doc
    <*> cast =<< M.look "lastLoginIp" doc
    <*> cast =<< M.look "createdAt" doc
    <*> cast =<< M.look "updatedAt" doc
    <*> cast =<< M.look "roles" doc
    <*> retur HM.empty
