{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | (Try) to be Generic Operation for DB operations
-- 

module Models.Internal.Types where

import Snap.Core
import           Database.MongoDB
import qualified Database.MongoDB as DB
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB
import Control.Monad.Trans

import           Application
import           Models.Exception
import           Models.Utils

-- | A simple generic Persistent handler.
-- 
-- 
class Persistent a where
  -- | Schema name
  --   Implement me like `getSchemaP _ = u "abc"`
  getSchemaP :: a -> Collection
  
  -- | Transform Data to MongoDB document type
  toMongoDocP :: a -> Document
  
  -- | Transform MongoDB document to perticular type
  fromMongoDocP :: Document -> IO a
  
  -- | Simple MongoDB Save Operation 
  -- 
  saveP :: a -> AppHandler a
  saveP x = do
            res <- eitherWithDB $ DB.save (getSchemaP x) (toMongoDocP x)
            either failureToUE (const $ return x) res
  
  -- | Fetch All items in the collection
  -- 
  getAllP :: a -> AppHandler [a]
  getAllP x  = do
               let selection = select [] (getSchemaP x)
               -- let selection = select [] (getSchemaP (undefined::a)) WHY IT FAILED.?             
               xs <- eitherWithDB $ rest =<< find (selection)
               liftIO $ mapM fromMongoDocP $ either (const []) id xs

