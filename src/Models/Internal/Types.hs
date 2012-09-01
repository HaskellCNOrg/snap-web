{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | (Try) to be Generic Operation for DB operations
-- 

module Models.Internal.Types where

import           Snap.Core
import           Snap
import           Database.MongoDB
import qualified Database.MongoDB as DB
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB
import           Control.Monad.Trans

import           Models.Internal.Exception

-- | A simple generic MongoDB Persistent class.
-- 
-- 
class MongoDBPersistent a where
  -- | Schema name
  --   Implement me like `getSchemaP _ = u "abc"`
  mongoColl :: a -> Collection
  
  -- | Transform Data to MongoDB document type
  toMongoDoc :: a -> Document
  
  -- | Transform MongoDB document to perticular type
  fromMongoDoc :: Document -> IO a
  
  -- | FIXME: Insert ID after save successfully. @see Topic.hs: 51
  -- | Simple MongoDB Save Operation 
  -- 
  mongoInsert :: (MonadIO m, MonadState app m, HasMongoDB app) 
           => a    -- ^ new model that will be save
           -> m a  -- ^ saved model with id.
  mongoInsert x = do
            res <- eitherWithDB $ DB.insert (mongoColl x) (toMongoDoc x)
            either failureToUE (const $ return x) res
  
  -- | Fetch All items in the collection
  -- 
  mongoFindAll :: (MonadIO m, MonadState app m, HasMongoDB app) 
             => a               -- ^ an empty model. (work around for the concern below.
             -> m [a]  -- ^ list of model data that has been retrieved.
  mongoFindAll x  = do
               let selection = select [] (mongoColl x)
               -- let selection = select [] (getSchemaP (undefined::a)) WHY IT FAILED.?             
               xs <- eitherWithDB $ rest =<< find (selection)
               liftIO $ mapM fromMongoDoc $ either (const []) id xs

