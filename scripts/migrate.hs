{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Control.Monad.CatchIO              (throw)
import Control.Exception (ErrorCall(..))
import Database.MongoDB
import Data.Text (Text)
import Control.Monad.Trans (liftIO)
import Data.Bson
import qualified Data.Bson as BSON
import Data.Baeson.Types
import Control.Applicative
import Models.User
import           Data.Baeson.Types
import           Data.Bson
import           Crypto.PasswordStore
import System.IO.Unsafe

main = run

run = do
    pipe <- runIOE $ connect (host "127.0.0.1")
    u <- access pipe slaveOk "node_club" findNodeUser
    t <- access pipe slaveOk "node_club" findNodeTopic
    r <- access pipe slaveOk "node_club" findNodeReply
    access pipe slaveOk "test-haskell" clearAll
    let datas = ( either (const []) id u
                , either (const []) id t
                , either (const []) id r)                          
    access pipe slaveOk "test-haskell" (doMigrate datas)
    close pipe >> print "done"

doMigrate (usr, t, r) = do
    let (xs, ys) = migUsers usr
    insertMany "users" xs
    insertMany "auth_user" ys
    insertMany "topics" t
    insertMany "replies" r
    where ts = map (merge ["tags" := Null]) t

clearAll :: Action IO ()
clearAll =
    delete (select [] "users")
    >> delete (select [] "auth_user")
    >> delete (select [] "topics")
    >> delete (select [] "replies")

migUsers :: [Document] -> ([Document], [Document])
migUsers docs = (xs, ys)
    where xs = map (merge xsOthers . map m1 . include ["_id", "name", "email", "url"]) docs
          ys = map (mergeys . map m2 . include ["_id", "email", "create_at", "update_at"]) docs
          m1 field = case field of
                     ("name" := v) -> ("display_name" := v)
                     x -> x
          m2 field = case field of
                     ("email" := v) -> ("login" := v)
                     ("create_at" := v) -> ("createdAt" := v)
                     ("update_at" := v) -> ("updatedAt" := v)
                     x -> x
          xsOthers = [ "active" := Bool False ]
          mergeys = merge ysOthers
          ysOthers = [ "activatedAt" := Null
                     , "suspendedAt" := Null
                     , "rememberToken" := Null
                     , "loginCount" .=  (0 :: Int)
                     , "userFailedLoginCount" .= (0 :: Int)
                     , "lockedOutUntil" := Null
                     , "currentLoginAt" := Null
                     , "lastLoginAt" := Null
                     , "currentLoginIp" := Null
                     , "lastLoginIp" := Null
                     , "roles" := Array []
                     , "password" := (Bin $ Binary $ mkp)
                  ]

mkp= unsafePerformIO $ do
    makePassword "12345678" 12
  

----------------------------------------------------------------------

findNodeUser :: Action IO [Document]
findNodeUser = rest =<< find (select [] "users")

findNodeTopic :: Action IO [Document]
findNodeTopic = rest =<< find (select [] "topics")

findNodeReply :: Action IO [Document]
findNodeReply = rest =<< find (select [] "replies")

