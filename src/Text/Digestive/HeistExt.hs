{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Text.Digestive.HeistExt where

import           Data.Text             (Text)
import           Text.Digestive
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml          as X


dfChildErrorListRef :: Monad m => View Text -> I.Splice m
dfChildErrorListRef view =
    return $ errorList (viewErrors view)
  where errorList :: [(Path, Text)] -> [X.Node]
        errorList []    = []
        errorList errs  = [X.Element "ul" [] $ map makeError errs]
        makeError (p:_, e) = X.Element "li" [("data-error", p)] [X.TextNode e]
