{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Routes
  ( routes 
  ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe

import           Application
import           Controllers.Home
import qualified Controllers.User as User
import qualified Controllers.Topic as Topic
import qualified Controllers.Reply as Reply

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
         [ ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]
