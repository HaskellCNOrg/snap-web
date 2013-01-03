{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.ReplySplices where

import           Control.Arrow         (second)
import           Control.Monad         (liftM)
import           Control.Monad.Trans
import           Data.Function         (on)
import           Data.List
import qualified Data.Map              as MP
import qualified Data.Text             as T
import           Heist
import qualified Heist.Interpreted as I

import           Application
import           Models.Reply
import           Models.User
import           Models.Utils
import           Views.MarkdownSplices
import           Views.UserSplices
import           Views.Utils

-- | A Reply and its own replies.
--
type ReplyWithReply = (Reply, [Reply])


------------------------------------------------------------------------------

-- | Reply with all children
--
allReplyPerTopicSplice :: [Reply] -> I.Splice AppHandler
allReplyPerTopicSplice xs = I.mapSplices replySpliceWithChildren (splitReplies xs)

-- | Display content of a reply as markdown.
--   Display content of a comment (reply of reply) as plain content.
--
replySpliceWithChildren :: ReplyWithReply -> I.Splice AppHandler
replySpliceWithChildren (r, rs) = do
    user <- findReplyAuthor r
    I.runChildrenWith $ [ ("replyEditable", hasEditPermissionSplice user)
                      , ("replyToReply", I.mapSplices replyToReplySplice rs)
                      , ("replyContentMD", markdownToHtmlSplice $ _replyContent r)
                      ]
                      ++ map (second I.textSplice) (replySpliceImpl r user)


------------------------------------------------------------------------------


-- | Just a Reply without any children
--
replyToReplySplice :: Reply -> I.Splice AppHandler
replyToReplySplice r = do
    user <- findReplyAuthor r
    I.runChildrenWith $ [ ("replyEditable", hasEditPermissionSplice user)
                      , ("replyContentMD", markdownToHtmlSplice $ _replyContent r)
                      ]
                      ++  map (second I.textSplice) (replySpliceImpl r user)


replySpliceImpl :: Reply
                -> User   -- ^ UserName
                -> [(T.Text, T.Text)]
replySpliceImpl r user =
                    [ ("replyAuthor", _userDisplayName user)
                    , ("replyAuthorId", sToText $ _replyAuthor r)
                    , ("replyId", getReplyId r)
                    , ("replyToTopicId", sToText $ _replyToTopicId r)
                    , ("replyToReplyId", objectIdToText $ _replyToReplyId r)
                    , ("replyCreateAt", formatUTCTime $ _replyCreateAt r)]
--                    , ("replyContent",  _replyContent r) ]


------------------------------------------------------------------------------

-- | Separate Reply by whether it is a reply or a reply of reply, then zip together.
--   FIXME:
--         1. a little complex
--         2. unit test
--
splitReplies :: [Reply] -> [ReplyWithReply]
splitReplies rs =
    map (g $ toMap $ grouyByToReplyId $ sortByToReplyId $ nonFirstLevelReply rs)
        (firstLevelReply rs)
    where sortByToReplyId  = sortBy (compare `on` toReplyId)
          grouyByToReplyId = groupBy ((==) `on` _replyToReplyId)
          toMap            = MP.fromList . map (\xs -> (toReplyId (head xs) , xs))
          toReplyId        = objectIdToText . _replyToReplyId
          g m r            = case MP.lookup (getReplyId r) m of
                               Nothing -> (r, [])
                               Just xs -> (r, xs)



-- | @Splice@ is type synonium as @Splice m = HeistT m Template@
--
findReplyAuthor :: Reply ->  HeistT AppHandler AppHandler User
findReplyAuthor reply = lift (findUser' reply)
                        where findUser' = findOneUser . _replyAuthor

findReplyAuthorName :: Reply
                       -> HeistT AppHandler AppHandler T.Text
findReplyAuthorName reply = liftM _userDisplayName (findReplyAuthor reply)
