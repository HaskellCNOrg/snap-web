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
-- FIXME: Cant make tight the Collection thus currently have to be parameter of save/get function.
-- 
class Persistent a where
  -- | Schema name
  -- getSchemaP :: Collection
  
  -- | Transform Data to MongoDB document type
  toMongoDocP :: a -> Document
  fromMongoDocP :: Document -> IO a
  
  -- | Simple MongoDB Save Operation 
  -- 
  saveP :: Collection -> a -> AppHandler a
  saveP c x = do
            res <- eitherWithDB $ DB.save c (toMongoDocP x)
            either failureToUE (const $ return x) res
  
  -- | Fetch All items in the collection
  -- 
  getAllP :: Collection -> AppHandler [a]
  getAllP c = do
            let selection = select [] c
            xs <- eitherWithDB $ rest =<< find (selection)
            liftIO $ mapM fromMongoDocP $ either (const []) id xs

