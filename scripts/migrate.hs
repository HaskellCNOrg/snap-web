{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Control.Monad.CatchIO              (throw)
import Control.Exception (ErrorCall(..))
import Database.MongoDB
import Data.Text (Text)
import qualified Data.Text.Encoding   as T
import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import qualified Data.Bson as BSON
import Control.Applicative
import Models.User
import           Data.Baeson.Types
import           Data.Bson
import           Crypto.PasswordStore
import System.IO.Unsafe
import System.Environment

main = run

run = do
    args <- getArgs
    let target = if null args then "test-migrate-nodeclub" else head args
    pipe <- runIOE $ connect (host "127.0.0.1")
    let dbNodeClub = accessNodeClub pipe
        dbTarget = accessHaskellCN pipe (T.pack target)
    u <- dbNodeClub findNodeUser
    t <- dbNodeClub findNodeTopic
    r <- dbNodeClub findNodeReply
    dbTarget clearAll
    let datas = ( either (const []) id u
                , either (const []) id t
                , either (const []) id r)
    dbTarget (doMigrate datas)
    close pipe >> print "done"

accessNodeClub pipe = access pipe slaveOk "node_club"
accessHaskellCN pipe = access pipe slaveOk

doMigrate (usr, t, r) = do
    let (xs, ys) = migUsers usr
    insertMany "users" xs
    insertMany "auth_user" ys
    insertMany "topics" t
    insertMany "replies" r
    --where ts = map (merge ["tags" := Null]) t

clearAll :: Action IO ()
clearAll =
    delete (select [] "users")
    >> delete (select [] "auth_user")
    >> delete (select [] "topics")
    >> delete (select [] "replies")

migUsers :: [Document] -> ([Document], [Document])
migUsers docs = (xs, ys)
    where xs = map (map m1 . include ["_id", "name", "email", "url"]) docs
          ys = map (mergeys . map m2 . password' . include ["_id", "email", "create_at", "update_at"]) docs
               -- ++
               -- (map (map password' . include ["email"]) docs)
          m1 field = case field of
                     ("name" := v) -> "display_name" := v
                     x -> x
          m2 field = case field of
                     ("email" := v) -> ("login" := v)
                     ("create_at" := v) -> ("createdAt" := v)
                     ("update_at" := v) -> ("updatedAt" := v)
                     x -> x
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
                  ]
          password' fields = let email = head $ include ["email"] fields
                                 pass = mkp2 email
                             in
                             fields ++ [pass]


mkp passwd = unsafePerformIO $ makePassword passwd 12

-- | use email as default password
--
mkp2 :: Field -> Field
mkp2 (_ := String v) = "password" := (Bin $ Binary $ mkp $ textToBS v)

textToBS = T.encodeUtf8

----------------------------------------------------------------------

findNodeUser :: Action IO [Document]
findNodeUser = rest =<< find (select [] "users")

findNodeTopic :: Action IO [Document]
findNodeTopic = rest =<< find (select [] "topics")

findNodeReply :: Action IO [Document]
findNodeReply = rest =<< find (select [] "replies")
