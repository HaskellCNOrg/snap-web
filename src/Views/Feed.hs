{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.Feed
where

import           Blaze.ByteString.Builder (Builder)
import qualified Data.Text                as T
import           Text.XmlHtml

import           Models.Topic
import           Models.Reply
import           Views.MarkdownSplices    (markdownToHtmlText)
import           Views.Utils

renderTopicFeed :: [Topic] -> Builder
renderTopicFeed ts =
    render $ XmlDocument UTF8 Nothing [root]
  where
    f = head ts
    namespace = "http://www.w3.org/2005/Atom"
    root = Element "feed" [("xmlns",namespace)]
        $ Element "title" [] [TextNode "HaskellCNOrg Topics"]
        -- TODO: retrieve host address here
        : Element "link" [("href", "/feed/topic"), ("rel", "self")] []
        : Element "link" [("href", "/")] []
        : Element "updated" [] [TextNode $ formatUTCTime $ _updateAt f]
        : map renderTopicEntry ts

renderTopicEntry :: Topic -> Node
renderTopicEntry t =
    Element "entry" []
    [ Element "title" [] [TextNode $ _title t]
    , Element "link" [("href", T.concat ["/topic/", getTopicId t])] []
    , Element "id" [] [TextNode $ T.concat ["/topic/", getTopicId t]]
    , Element "published" [] [TextNode $ formatUTCTime $ _createAt t]
    , Element "content" [("type","html")] [TextNode $ markdownToHtmlText $ _content t]
    ]

renderReplyFeed :: [Reply] -> [Topic] -> Builder
renderReplyFeed rs ts = do
    render $ XmlDocument UTF8 Nothing [root]
  where
    f = head rs
    namespace = "http://www.w3.org/2005/Atom"
    root = Element "feed" [("xmlns",namespace)]
        $ Element "title" [] [TextNode "HaskellCNOrg Comments"]
        -- TODO: retrieve host address here
        : Element "link" [("href", "/feed/comment"), ("rel", "self")] []
        : Element "link" [("href", "/")] []
        : Element "updated" [] [TextNode $ formatUTCTime $ _replyCreateAt f]
        : zipWith renderReplyEntry rs ts

renderReplyEntry :: Reply -> Topic -> Node
renderReplyEntry r t =
    Element "entry" []
    [ Element "title" [] [TextNode $ T.concat ["Comments on ", _title t]]
    , Element "link" [("href", T.concat ["/topic/", getTopicId t])] []
    , Element "id" [] [TextNode $ T.concat ["/topic/", getReplyId r]]
    , Element "published" [] [TextNode $ formatUTCTime $ _replyCreateAt r]
    , Element "content" [("type","html")] [TextNode $ markdownToHtmlText $ _replyContent r]
    ]
