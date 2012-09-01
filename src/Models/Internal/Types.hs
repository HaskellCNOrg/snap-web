{-# LANGUAGE OverloadedStrings #-}

-- | (Try) to be Generic Operation for DB operations
-- 

module Models.Internal.Types where

import           Control.Monad.Trans
import           Database.MongoDB
import qualified Data.Text as T
import           Snap
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB
import qualified Database.MongoDB as DB

import           Models.Internal.Exception

-- | A simple generic MongoDB Persistent class.
-- 
-- 
class MongoDBPersistent a where
  -- | Schema name
  --   Implement me like `getSchemaP _ = u "abc"`
  mongoColl :: a -> Collection
  
  -- | Transform Data to MongoDB document type
  --
  toMongoDoc :: a -> Document
  
  -- | Transform MongoDB document to perticular type
  --
  fromMongoDoc :: Document -> IO a
  
  -- | Get ObejectId
  -- 
  mongoGetId :: a -> Maybe ObjectId
  
  -- DB Operations --
  
  -- | FIXME: Insert ID after save successfully. @see Topic.hs: 51
  -- | Simple MongoDB Save Operation 
  -- 
  mongoInsert :: (MonadIO m, MonadState app m, HasMongoDB app) 
           => a    -- ^ new model that will be save
           -> m a  -- ^ saved model with id.
  mongoInsert x = eitherWithDB (DB.insert (mongoColl x) (toMongoDoc x))
                  >>= either failureToUE (return . mongoInsertId x)
  
  -- | Update ID field of model after insert to mongoDB successfully.
  -- 
  mongoInsertId :: a      -- ^ original data that about to be save.
                -> Value  -- ^ return value after mongoDB.insert, should be a ObjectID.
                -> a      -- ^ updated data with ID filed get updated.
  
  -- | Fetch All items in the collection
  -- 
  -- WHY IT FAILED: let selection = select [] (getSchemaP (undefined::a))
  -- 
  mongoFindAll :: (MonadIO m, MonadState app m, HasMongoDB app)
             => a       -- ^ an empty model. (work around for the concern below.
             -> m [a]   -- ^ list of model data that has been retrieved.
  mongoFindAll x  = 
    eitherWithDB (rest =<< find (select [] (mongoColl x)))
    >>= liftIO . mapM fromMongoDoc . either (const []) id
  
  -- | Find One item.
  --
  mongoFindOne :: (MonadIO m, MonadState app m, HasMongoDB app)
                  => a
                  -> m a
  mongoFindOne x =
    eitherWithDB (fetch (select ("_id" =? mongoGetId x) (mongoColl x)))
    >>= either failureToUE (liftIO . fromMongoDoc)

  -- | FIXME: is able find some via list of IDs???
  --
  mongoFindSome :: (MonadIO m, MonadState app m, HasMongoDB app)
                  => a
                  -> [ObjectId] 
                  -> m [a]
  mongoFindSome _ [] = return []
  -- THIS DOESNT WORK YET --
  mongoFindSome x ids@(x1:xs) = do
    let oid = mongoGetId x
        coll = mongoColl x
        selection = select [ "_id" =: ids ] coll
    eitherWithDB $ rest =<< find selection
    >>= liftIO . mapM fromMongoDoc . either (const []) id

{-
--js =  Javascript [] ("{tags $in" `T.append` (T.pack $ show ids) `T.append` "}")
-}

