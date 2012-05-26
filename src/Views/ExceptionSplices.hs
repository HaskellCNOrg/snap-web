{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.ExceptionSplices where

import           Text.Templating.Heist

import           Models.Exception
import           Views.Types
import Application
------------------------------------------------------------------------------
          
--renderUE :: Maybe UserException -> Splice AppHandler          
--renderUE Nothing = return []
--renderUE (Just a) = runChildrenWithText [ ("exceptionValue", showUE a) ]
