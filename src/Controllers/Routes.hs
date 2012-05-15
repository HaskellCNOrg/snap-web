{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Routes
  ( routes 
  ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe

import           Application
import           Controllers.Home
import           Controllers.Login

routes :: [(ByteString, Handler App App ())]
routes = [ ("/",             index)
         , ("/index",        index)
         ]
         <|>
         [ ("/signup",  method GET signupG <|> method POST signupP)
         , ("/signin",  signin)
         , ("/signout", method GET signoutG)           
         ]
         <|>
         [ ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]
