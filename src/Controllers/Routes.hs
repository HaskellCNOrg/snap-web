{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Routes
  ( routes
  ) where

import           Control.Applicative
import           Data.ByteString       (ByteString)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe

import           Application
import qualified Controllers.Exception as Ex
import           Controllers.Home
import qualified Controllers.Reply     as Reply
import qualified Controllers.Tag       as Tag
import qualified Controllers.Topic     as Topic
import qualified Controllers.User      as User
import qualified Controllers.Feed      as Feed

routes :: [(ByteString, Handler App App ())]
routes = [ ("/",             index)
         , ("/index",        index)
         ]
         <|>
         User.routes
         <|>
         Topic.routes
         <|>
         Reply.routes
         <|>
         Tag.routes
         <|>
         Feed.routes
         <|>
         [ ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]
         <|> Ex.routes
