{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | The work is mainly steal from https://gist.github.com/2725402
--
--

module Snap.Snaplet.Auth.Backends.MongoDB where


import Control.Exception (Exception)
import Data.Typeable (Typeable)
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.CatchIO       (throw)
import           Control.Monad.Error
import           Data.Baeson.Types
import qualified Data.Bson                   as BSON
import qualified Data.Configurator           as C
import qualified Data.HashMap.Lazy           as HM
import Control.Lens hiding ((.=), Action)
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Database.MongoDB            as M
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import qualified Snap.Snaplet.MongoDB        as SM
import           Snap.Snaplet.Session
import           System.IO.Pool              (Pool, aResource)
import           Web.ClientSession

import           Database.MongoDB            (AccessMode (UnconfirmedWrites),
                                              Action, Database, Failure (..),
                                              Pipe)


------------------------------------------------------------------------------
-- | Simple function to get auth settings from a config file. All options
-- are optional and default to what's in defAuthSettings if not supplied.
settingsFromConfig :: Initializer b (AuthManager b) AuthSettings
settingsFromConfig = do
    config <- getSnapletUserConfig
    minPasswordLen' <- liftIO $ C.lookup config "minPasswordLen"
    let pw = maybe id (\x s -> s { asMinPasswdLen = x }) minPasswordLen'
    rememberCookie' <- liftIO $ C.lookup config "rememberCookie"
    let rc = maybe id (\x s -> s { asRememberCookieName = x }) rememberCookie'
    rememberPeriod' <- liftIO $ C.lookup config "rememberPeriod"
    let rp = maybe id (\x s -> s { asRememberPeriod = Just x }) rememberPeriod'
    lockout' <- liftIO $ C.lookup config "lockout"
    let lo = maybe id (\x s -> s { asLockout = Just (second fromInteger x) })
                   lockout'
    siteKey' <- liftIO (C.lookup config "db.siteKey")

    -- very wired that not able to lookup anything from config even exists.

    let sk = maybe id (\x s -> s { asSiteKey = x }) siteKey'
    return $ (pw . rc . rp . lo . sk) defAuthSettings


------------------------------------------------------------------------------
-- | Initializer for the MongoDB backend to the auth snaplet.
--
initMongoAuth :: SnapletLens b SessionManager
                 -> Snaplet SM.MongoDB
                 -> Maybe String    -- ^ Site Key path
                 -> SnapletInit b (AuthManager b)
initMongoAuth sess db sk = makeSnaplet "mongodb-auth" desc Nothing $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "auth_user" config "authCollection"
    authSettings <- settingsFromConfig
    key <- liftIO $ getKey (fromMaybe (asSiteKey authSettings) sk)
    let
      lens' = db ^. snapletValue
      manager = MongoBackend authTable (SM.mongoDatabase lens')
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
    --datadir = "/resources/auth" :: String


data MongoBackend = MongoBackend
    { mongoCollection :: M.Collection
    , mongoDatabase :: Database
    , mongoPool :: Pool IOError Pipe
    }

accessMode :: AccessMode
accessMode = UnconfirmedWrites

-- | default UserId is Nothing thus set to same as UserLogin
--
mongoSave :: MongoBackend -> AuthUser -> IO (Either AuthFailure AuthUser)
mongoSave mong usr =
  case userId usr of
      Nothing -> insertUser' usr
      _       -> saveUser' usr
  where  insertUser' u = do
                        res <- dbQuery mong $ M.insert (mongoCollection mong) $ usrToMong u
                        case res of
                            Left (WriteFailure 11000 _) -> return $ Left DuplicateLogin
                            Left v  -> throwBE v
                            Right r -> return $ Right (insertId' r)
         insertId' x = usr { userId = objectIdToUserId <$> BSON.cast' x}
         saveUser' u = do
                       res <- dbQuery mong $ M.save (mongoCollection mong) $ usrToMong u
                       case res of
                           Left (WriteFailure 11000 _) -> return $ Left DuplicateLogin
                           Left v  -> throwBE v
                           _ -> return $ Right u
         throwBE = return . Left . AuthError . show


mongoAct :: MongoBackend -> Action IO a -> (a -> IO b) -> IO b
mongoAct mong act' conv = do
  res <- dbQuery mong act'
  case res of
    Left e -> throw $ AuthMongoException $ show e
    Right r -> conv r

mongoLookup :: MongoBackend -> M.Document -> IO (Maybe AuthUser)
mongoLookup mong doc =
  mongoAct mong act' conv
  where
    act' = M.findOne $ M.select doc (mongoCollection mong)
    conv = maybe (return Nothing) parseUsr
    parseUsr = liftM Just . usrFromMongThrow

mongoLookupByLogin :: MongoBackend -> Text -> IO (Maybe AuthUser)
mongoLookupByLogin mong login = mongoLookup mong ["login" .= login]

mongoLookupById :: MongoBackend -> UserId -> IO (Maybe AuthUser)
mongoLookupById mong uid = mongoLookup mong ["_id" .= uid]

mongoLookupByToken :: MongoBackend -> Text -> IO (Maybe AuthUser)
mongoLookupByToken mong tok = mongoLookup mong ["rememberToken" .= tok]

mongoDestroy :: MongoBackend -> AuthUser -> IO ()
mongoDestroy mong usr =
  maybe (return ()) actonid $ userId usr
  where
    coll = mongoCollection mong
    actonid uid = mongoAct mong (act' uid) return
    act' uid =  M.deleteOne (M.select ["_id" .= uid] coll)

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

-- | UserId is stored as ObjectId in mongoDB.
--
instance FromBSON UserId where
  fromBSON (M.ObjId oid) = pure $ objectIdToUserId oid

objectIdToUserId :: BSON.ObjectId -> UserId
objectIdToUserId = UserId . T.pack . show

userIdToObjectId :: UserId -> BSON.ObjectId
userIdToObjectId = read . T.unpack . unUid

-- | Transform UserId to ObjectId
--
instance ToBSON UserId where
  toBSON = M.ObjId . read . T.unpack . unUid

-- | Transform UserLogin name to UUID for unique id.
--
userLoginToUUID :: Text -> M.UUID
userLoginToUUID  = M.UUID . T.encodeUtf8

usrToMong :: AuthUser -> M.Document
usrToMong usr = case userId usr of
                    Nothing -> docs
                    Just x  -> ("_id" .= x ) : docs
                where docs = [ "login" .= userLogin usr
                             , "email" .= userEmail usr
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
                             , "resetToken" .= userResetToken usr
                             , "resetRequestedAt" .= userResetRequestedAt usr
                             , "roles" .= userRoles usr
                             ]

usrFromMong :: M.Document -> Parser AuthUser
usrFromMong d = AuthUser
                <$> d .:? "_id"
                <*> d .: "login"
                <*> d .:? "email"
                <*> d .:? "password"
                <*> d .:? "activatedAt"
                <*> d .:? "suspendedAt"
                <*> d .:? "rememberToken"
                <*> d .: "loginCount"
                <*> d .: "userFailedLoginCount"
                <*> d .:? "lockedOutUntil"
                <*> d .:? "currentLoginAt"
                <*> d .:? "lastLoginAt"
                <*> d .:? "currentLoginIp"
                <*> d .:? "lastLoginIp"
                <*> d .:? "createdAt"
                <*> d .:? "updatedAt"
                <*> d .:? "resetToken"
                <*> d .:? "resetRequestedAt"
                <*> d .: "roles" .!= []
                <*> pure HM.empty

usrFromMongThrow :: M.Document -> IO AuthUser
usrFromMongThrow d =
  case parseEither usrFromMong d of
    Left e -> throw $ ParseDocumentException e
    Right r -> return r

data AuthMongoException = ParseDocumentException String
                          | AuthMongoException String
                        deriving (Show, Typeable)

instance Exception AuthMongoException
