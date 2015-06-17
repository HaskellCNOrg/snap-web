{-# LANGUAGE OverloadedStrings #-}

module Views.Types where

import           Application
import           Heist
import qualified Heist.Interpreted as I
import           Models.Exception

-- | This class is born because when do MonadIO.try with models functions,
--   its return type is `Either exception data`. Hence make a generic render.
--   See also the helper @eitherToSplices@.
--
class SpliceRenderable a where
    toSplice :: a -> I.Splice AppHandler
    getSubTitle :: a -> I.Splice AppHandler
    getSubTitle _ = I.textSplice ""

--------------------------------------------------------------

-- | FIXME: What if at some exception case, should both should content and error??
--
eitherToSplices :: SpliceRenderable a => Either UserException a -> Splices (I.Splice AppHandler)
eitherToSplices (Left l) = do
  "ifFound" ## return []
  "ifNotFound" ## toSplice l
  "subTitle" ## getSubTitle l

eitherToSplices (Right r) = do
  "ifFound" ## toSplice r
  "ifNotFound" ## return []
  "subTitle" ## getSubTitle r

--------------------------------------------------------------

instance SpliceRenderable UserException where
    toSplice a = I.runChildrenWithText  ("exceptionValue" ## showUE a)
    getSubTitle _ = I.textSplice "Application Error"
