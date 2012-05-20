{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.ExceptionSplices where

import           Text.Templating.Heist

import           Application
import           Models.Exception

------------------------------------------------------------------------------
                    
-- | db.Topic.findAll
-- 

renderUE:: Maybe UserException -> Splice AppHandler
renderUE Nothing  = return []
renderUE (Just e) = runChildrenWithText $ 
                    [ ("exceptionValue", showUE e) ]
