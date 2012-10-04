{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | (Try) to be Generic Operation for DB operations
--

module Models.Internal.Types where

import           Control.Monad.Trans
import           Data.Bson
import           Database.MongoDB
import qualified Database.MongoDB          as DB
import           Snap
import           Snap.Snaplet.MongoDB

import           Models.Internal.Exception
import           Models.Internal.JSON


--------------------------------------------------------------------------------

-- | A simple generic MongoDB Persistent class.
--
--
class MongoDBPersistent a where

  -- | Schema name. Implement me like `getSchemaP _ = u "abc"`
  --
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

  -- | Update ID field of model after insert to mongoDB successfully.
  --
  mongoInsertId :: a      -- ^ original data that about to be save.
                -> Value  -- ^ return value after mongoDB.insert, should be a ObjectID.
                -> a      -- ^ updated data with ID filed get updated.


--------------------------------------------------------------------------------


-- | Simple MongoDB Insert Operation
--
mongoInsert :: (MonadIO m, MonadState app m, HasMongoDB app, MongoDBPersistent a)
               => a    -- ^ new model that will be save
               -> m a  -- ^ saved model with id.
mongoInsert x = eitherWithDB (DB.insert (mongoColl x) (toMongoDoc x))
                >>= either failureToUE (return . mongoInsertId x)


-- | Simple MongoDB Save Operation
--
-- meaning insert it if its new (has no "_id" field) or update it if its not new (has "_id" field)
--
-- MAYBE:
-- 1. better to make sure _id exists because Nothing objectId will cause error other when viewing.
-- 
mongoSave :: (MonadIO m, MonadState app m, HasMongoDB app, MongoDBPersistent a)
             => a    -- ^ new model that will be save
             -> m a  -- ^ saved model with id.
mongoSave x = eitherWithDB (DB.save (mongoColl x) (toMongoDoc x))
              >>= either failureToUE (const $ return x)


-- | Fetch All items in the collection
--
-- ?? WHY IT FAILED: let selection = select [] (getSchemaP (undefined::a))
--
mongoFindAll :: (MonadIO m, MonadState app m, HasMongoDB app, MongoDBPersistent a)
                => a       -- ^ an empty model. (work around for the concern below.
                -> m [a]   -- ^ list of model data that has been retrieved.
mongoFindAll x  =
  eitherWithDB (rest =<< find (select [] (mongoColl x)))
  >>= liftIO . mapM fromMongoDoc . either (const []) id


-- | Fetch All items in the collection per user defined selector(query).
--
mongoFindAllBy :: (MonadIO m, MonadState app m, HasMongoDB app, MongoDBPersistent a)
                  => a       -- ^ an empty model. (work around for the concern below.
                  -> Query
                  -> m [a]   -- ^ list of model data that has been retrieved.
mongoFindAllBy _ query =
  eitherWithDB (rest =<< find query)
  >>= liftIO . mapM fromMongoDoc . either (const []) id


-- | Find One item.
--
mongoFindById :: (MonadIO m, MonadState app m, HasMongoDB app, MongoDBPersistent a)
                 => a
                 -> m a
mongoFindById x =
  eitherWithDB (fetch (select ("_id" =? mongoGetId x) (mongoColl x)))
  >>= either failureToUE (liftIO . fromMongoDoc)


-- | Find some via list of IDs.
--
mongoFindIds :: (MonadIO m, MonadState app m, HasMongoDB app, MongoDBPersistent a)
                 => a
                 -> [ObjectId]
                 -> m [a]
mongoFindIds = mongoFindSomeBy "_id"


-- | Find some via list of certain column name.
--   MAYBE: this turns out to be complicated.
--
mongoFindSomeBy :: (MonadIO m, MonadState app m, HasMongoDB app, MongoDBPersistent a, Val b)
                 => Label     -- ^ Column name
                 -> a         -- ^ The Persistent target
                 -> [b]       -- ^ List of values
                 -> m [a]
mongoFindSomeBy _ _ [] = return []
mongoFindSomeBy l x xs = do
    let collect = mongoColl x
        selIn = selectIn xs
        sel = select [ l =: selIn ] collect
    eitherWithDB $ rest =<< find sel
    >>= liftIO . mapM fromMongoDoc . either (const []) id


-- | Prepare "$in" statement for query.
--
selectIn :: Val a => [a] -> Document
selectIn xs = ["$in" =: xs]
