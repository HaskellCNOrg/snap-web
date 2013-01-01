{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.Feed
where

import           Blaze.ByteString.Builder   (Builder)
import qualified Data.Text      as T
import Text.Pandoc
import Text.XmlHtml

import Models.Topic
import Models.Utils
import Views.MarkdownSplices                (markdownToHtmlString)
import Views.Utils

renderFeed :: [Topic] -> Builder
renderFeed ts = 
    render $ XmlDocument UTF8 Nothing [root]
  where
    f = head ts
    namespace = "http://www.w3.org/2005/Atom"
    root = Element "feed" [("xmlns",namespace)]  
        $ Element "title" [] [TextNode "HaskellCNOrg"]
        -- TODO: retrieve host address here
        : Element "link" [("href", "/feed/topic"), ("rel", "self")] []
        : Element "link" [("href", "/")] []
        : Element "updated" [] [TextNode $ formatUTCTime $ _updateAt f]
        : map renderEntry ts

renderEntry :: Topic -> Node
renderEntry t = 
    Element "entry" [] 
    $ Element "title" [] [TextNode $ _title t]
    : Element "link" [("href", T.concat ["/topic/", getTopicId t])] []
    : Element "id" [] [TextNode $ T.concat ["/topic/", getTopicId t]]
    : Element "published" [] [TextNode $ formatUTCTime $ _updateAt t]
    : Element "content" [("type","html")] [TextNode $ htmlText $ _content t]
    : []
  where
    htmlText = bsToText . markdownToHtmlString
