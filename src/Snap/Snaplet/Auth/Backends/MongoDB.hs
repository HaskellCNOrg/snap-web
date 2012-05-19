{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards   #-}

{-
FROM: https://gist.github.com/2725402
-}

module Snap.Snaplet.Auth.Backends.MongoDB where


import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.CatchIO(throw)
import Control.Monad.Error
import Data.Baeson.Types
import Data.Lens.Lazy
import Data.Maybe
import Data.Text(Text)
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
--import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import qualified Database.MongoDB as M
import qualified Snap.Snaplet.MongoDB as SM
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Common
import System.IO.Pool (Pool, Factory (Factory), newPool, aResource)
import Web.ClientSession

import Database.MongoDB (Database, Host, Pipe, (=:),
                                   AccessMode (UnconfirmedWrites),
                                   close, isClosed, connect, Action,
                                   Failure(..), access)


------------------------------------------------------------------------------
-- | Simple function to get auth settings from a config file. All options
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
    siteKey <- liftIO (C.lookup config "siteKey")
    -- ^ very wired that not able to lookup anything from config even exists.

    let sk = maybe id (\x s -> s { asSiteKey = x }) siteKey
    return $ (pw . rc . rp . lo . sk) defAuthSettings


------------------------------------------------------------------------------
-- | Initializer for the MongoDB backend to the auth snaplet.
--
initMongoAuth :: Lens b (Snaplet SessionManager)
                 -> Snaplet SM.MongoDB
                 -> Maybe String    -- ^ Site Key path
                 -> SnapletInit b (AuthManager b)
initMongoAuth sess db sk = makeSnaplet "mongodb-auth" desc Nothing $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "auth_user" config "authCollection"
    authSettings <- settingsFromConfig
    key <- liftIO $ getKey (fromMaybe (asSiteKey authSettings) sk)
    let
      lens' = getL snapletValue db
      manager = MongoBackend (M.u authTable) (SM.mongoDatabase lens')
                (SM.mongoPool lens')
    rng <- liftIO mkRNG
    return AuthManager
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
    datadir = "/resources/auth" :: String


data MongoBackend = MongoBackend
    { mongoCollection :: M.Collection
    , mongoDatabase :: Database
    , mongoPool :: Pool IOError Pipe
    }

accessMode = UnconfirmedWrites

-- | default UserId is Nothing thus set to same as UserLogin
-- 
mongoSave :: MongoBackend -> AuthUser -> IO AuthUser
mongoSave mong usr = do
  let usr' = case userId usr of
               Nothing -> usr {userId = Just . UserId $ userLogin usr}
               _       -> usr
  res <- dbQuery mong $ M.save (mongoCollection mong) $ usrToMong usr'
  case res of
    Left (WriteFailure 11000 msg) -> throw DuplicateLogin
    Left v -> throw $ BackendError $ show v
    Right _ -> return usr'

mongoAct :: MongoBackend -> Action IO a -> (a ->IO b) -> IO b
mongoAct mong act conv = do
  res <- dbQuery mong act
  case res of
    Left e -> throw $ BackendError $ show e
    Right r -> conv r

mongoLookup :: MongoBackend -> M.Document -> IO (Maybe AuthUser)
mongoLookup mong doc =
  mongoAct mong act conv
  where
    act = M.findOne $ M.select doc (mongoCollection mong)
    conv = maybe (return Nothing) parseUsr
    parseUsr = liftM Just . usrFromMongThrow

mongoLookupByLogin :: MongoBackend -> Text -> IO (Maybe AuthUser)
mongoLookupByLogin mong login = mongoLookup mong ["login" .= login]

mongoLookupById :: MongoBackend -> UserId -> IO (Maybe AuthUser)
mongoLookupById mong uid = mongoLookup mong ["_id" .= uid]

mongoLookupByToken :: MongoBackend -> Text -> IO (Maybe AuthUser)
mongoLookupByToken mong tok = mongoLookup mong ["rememberToken" .= tok]

mongoDestroy :: MongoBackend -> AuthUser -> IO ()
mongoDestroy mong usr = do
  maybe (return ()) actonid $ userId usr
  where
    coll = mongoCollection mong
    actonid uid = mongoAct mong (act uid) return
    act uid =  M.deleteOne (M.select ["_id" .= uid] coll)

instance IAuthBackend MongoBackend where
  save = mongoSave
  lookupByUserId = mongoLookupById
  lookupByLogin = mongoLookupByLogin
  lookupByRememberToken = mongoLookupByToken
  destroy = mongoDestroy

dbQuery :: (MonadIO m)
           => MongoBackend
           -> Action IO a
           -> m (Either Failure a)
dbQuery mong action = do
  let
    pool = mongoPool mong
    database = mongoDatabase mong
    mode = accessMode
  ep <- liftIO $ runErrorT $ aResource pool
  case ep of
    Left  err -> return $ Left $ ConnectionFailure err
    Right pip -> liftIO $ M.access pip mode database action


instance ToBSON Password where
  toBSON (Encrypted p) = toBSON p
  toBSON _ = error "Can't store unencrypted password"

instance FromBSON Password where
  fromBSON v = Encrypted <$> fromBSON v

instance ToBSON Role where
  toBSON (Role r) = toBSON $ T.decodeUtf8 r

instance FromBSON Role where
  fromBSON v = Role . T.encodeUtf8 <$> fromBSON v

-- | UserId is stored as UUID in mongoDB.
--
instance FromBSON UserId where
  fromBSON (M.Uuid (M.UUID v)) = pure $ UserId $ T.decodeUtf8 v

instance ToBSON UserId where
  toBSON = M.Uuid . M.UUID . T.encodeUtf8 . unUid

-- | Transform UserLogin name to UUID for unique id.
--
userLoginToUUID :: Text -> M.UUID
userLoginToUUID  = M.UUID . T.encodeUtf8

usrToMong :: AuthUser -> M.Document
usrToMong usr = [ "_id" .= userId usr
                , "login" .= userLogin usr
                , "password" .= userPassword usr
                , "activatedAt" .= userActivatedAt usr
                , "suspendedAt" .= userSuspendedAt usr
                , "rememberToken" .= userRememberToken usr
                , "loginCount" .= userLoginCount usr
                , "userFailedLoginCount" .= userFailedLoginCount usr
                , "lockedOutUntil" .= userLockedOutUntil usr
                , "currentLoginAt" .= userCurrentLoginAt usr
                , "lastLoginAt" .= userLastLoginAt usr
                , "currentLoginIp" .= userCurrentLoginIp usr
                , "lastLoginIp" .= userLastLoginIp usr
                , "createdAt" .= userCreatedAt usr
                , "updatedAt" .= userUpdatedAt usr
                , "roles" .= userRoles usr
                ]
usrFromMong :: M.Document -> Parser AuthUser
usrFromMong d = AuthUser
                <$> d .: "_id"
                <*> d .: "login"
                <*> d .:? "password"
                <*> d .: "activatedAt"
                <*> d .: "suspendedAt"
                <*> d .: "rememberToken"
                <*> d .: "loginCount"
                <*> d .: "userFailedLoginCount"
                <*> d .: "lockedOutUntil"
                <*> d .: "currentLoginAt"
                <*> d .: "lastLoginAt"
                <*> d .: "currentLoginIp"
                <*> d .: "lastLoginIp"
                <*> d .: "createdAt"
                <*> d .: "updatedAt"
                <*> d .: "roles" .!= []
                <*> pure HM.empty

usrFromMongThrow :: M.Document -> IO AuthUser
usrFromMongThrow d =  case parseEither usrFromMong d of
  Left e -> throw $ BackendError $ show e
  Right r -> return r