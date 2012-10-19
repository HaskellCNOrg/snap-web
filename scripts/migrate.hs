{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Control.Monad.CatchIO              (throw)
import Control.Exception (ErrorCall(..))
import Database.MongoDB
import Data.Text (Text)
import Control.Monad.Trans (liftIO)
import Data.Bson
import Control.Applicative
import Models.User
import           Data.Baeson.Types
import           Data.Bson

main = run

run = do
    pipe <- runIOE $ connect (host "127.0.0.1")
    e <- access pipe slaveOk "node_club" run2
    case e of
      Left x -> close pipe >> print x
      Right usr -> access pipe slaveOk "test-haskell" (run1 usr) >> close pipe >> print ""


run1 docs = do
    clearUsers
    let users = map (map tr . include ["_id", "pass", "loginname", "email", "url"]) docs
    insertMany "users" users
  where tr field = case field of
                     ("pass" := v) -> ("password" := v)
                     ("name" := v) -> ("display_name" := v)
                     x -> x
  
run2 :: Action IO [Document]
run2 = do
    users
    -- >>= liftIO . migrateUser

users :: Action IO [Document]
users = rest =<< find (select [] "users")

clearUsers :: Action IO ()
clearUsers =
    delete (select [] "users")
    >> delete (select [] "auth_user")

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $
                       putStrLn title
                       -- >> mapM_ (print . exclude ["_id"]) docs
                       >> migrateUser docs
                       >>= print

-- | haskellcn has two coll for user: auth-user, users
--

data NCUser = NCUser { __email :: Text
                     , __passwd :: Text
                     , __loginname :: Text                                   
                     } deriving (Show)

ncUserDocParser' d = NCUser
                    <$> d .: "email"
                    <*> d .: "pass"
                    <*> d .: "loginname"
ncUserDocParser f d = case parseEither f d of
    Left e  -> throw $ ErrorCall "parser NC User Error"
    Right r -> return r

migrateUser :: [Document] -> IO [NCUser]
migrateUser doc = do
                  usrs <- mapM (ncUserDocParser ncUserDocParser') doc
                  return usrs
