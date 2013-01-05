{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.Feed
where

import           Blaze.ByteString.Builder (Builder)
import qualified Data.Text                as T
import           Text.XmlHtml

import           Models.Feed
import           Views.MarkdownSplices    (markdownToHtmlText)
import           Views.Utils              (formatUTCTime)

renderFeed :: Feed -> Builder
renderFeed f = 
    render $ XmlDocument UTF8 Nothing [root]
  where
    namespace = "http://www.w3.org/2005/Atom"
    root = Element "feed" [("xmlns", namespace)]
         $ Element "title" [] [TextNode $ feedTitle f]
         : Element "link" [("href", feedLinkSelf f), ("rel", "self")] []
         : Element "link" [("href", feedLinkHome f)] []
         : Element "updated" [] [TextNode $ formatUTCTime $ feedUpdated f]
         : map renderFeedEntry (feedEntries f)

renderFeedEntry :: FeedEntry -> Node
renderFeedEntry e = 
    Element "entry" []
    [ Element "title" [] [TextNode $ feedEntryTitle e]
    , Element "link" [("href", feedEntryLink e)] []
    , Element "id" [] [TextNode $ feedEntryId e]
    , Element "published" [] [TextNode $ formatUTCTime $ feedEntryPublished e]
    , Element "content" [("type","html")] [TextNode $ markdownToHtmlText $ feedEntryContent e]
    ]
    
