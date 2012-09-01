{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Text.Digestive.HeistExt where

import           Text.Digestive
import           Text.Templating.Heist
import Data.Text (Text)
import qualified Text.XmlHtml as X



dfChildErrorListRef :: Monad m => View Text -> Splice m
dfChildErrorListRef view =
    return $ errorList (viewErrors view)
  where errorList :: [(Path, Text)] -> [X.Node]
        errorList []    = []
        errorList errs  = [X.Element "ul" [] $ map makeError errs]
                           where makeError (p:_, e) = X.Element "li" [("data-error", p)] [X.TextNode e]

