{-# LANGUAGE OverloadedStrings #-}

module Views.Types where

import qualified Data.Text as T
import Application
import           Text.Templating.Heist
import Models.Exception

-- | This class is born because when do MonadIO.try with models functions,
--   its return type is `Either exception data`. Hence make a generic render.
--  
class SpliceRenderable a where
    toSplice :: a -> Splice AppHandler

--------------------------------------------------------------


eitherToSplices :: SpliceRenderable a => Either UserException a -> [(T.Text, Splice AppHandler)]
eitherToSplices (Left l) = [ ("ifFound", return [])
                         , ("ifNotFound", toSplice l) ]

eitherToSplices (Right r) = [ ("ifFound", toSplice r)
                          , ("ifNotFound", return []) ]


--------------------------------------------------------------


instance SpliceRenderable UserException where
    toSplice a = runChildrenWithText [ ("exceptionValue", showUE a) ]

