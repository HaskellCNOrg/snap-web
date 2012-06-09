{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.ReplySplices
       ( allReplyPerTopicSplice 
       , replySplice ) where

import           Control.Arrow (second)
import           Control.Monad.Trans
import           Data.Maybe (fromJust)
import           Data.Function (on)
import           Text.Templating.Heist
import           Data.List
import qualified Data.ByteString as BS
import qualified Data.Map as MP
import qualified Data.Text as T


import Application
import Models.Exception
import Models.Topic
import Models.Reply
import Models.User
import Views.MarkdownSplices
import Views.Types
import Views.Utils
import Models.Utils

-- | A Reply and its own replies.
-- 
type ReplyWithReply = (Reply, [Reply])

type ReplyMap = MP.Map T.Text [Reply]

------------------------------------------------------------------------------

-- | Reply with all children
-- 
allReplyPerTopicSplice :: [Reply] -> Splice AppHandler
allReplyPerTopicSplice xs = mapSplices replySpliceWithChildren (splitReplies xs)

replySpliceWithChildren :: ReplyWithReply -> Splice AppHandler
replySpliceWithChildren (r, rs) = do
    usr <- findReplyAuthor r
    runChildrenWith $ ("replyToReply", mapSplices replySplice rs) : map (second textSplice) (replySpliceImpl r usr)


------------------------------------------------------------------------------


-- | Just a Reply without any children
-- 
replySplice :: Reply -> Splice AppHandler
replySplice r = findReplyAuthor r >>= runChildrenWithText . replySpliceImpl r


replySpliceImpl :: Reply -> User -> [(T.Text, T.Text)]
replySpliceImpl r usr =  
                    [ ("replyAuthor",  _userDisplayName usr)
                    , ("replyId", getReplyId r)
                    , ("replyToTopicId", sToText $ _replyToTopicId r)
                    , ("replyToReplyId", objectIdToText $ _replyToReplyId r)
                    , ("replyCreateAt", formatUTCTime $ _replyCreateAt r)
                    , ("replyContent", _replyContent r) ]


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
findReplyAuthor :: Reply -> HeistT AppHandler User
findReplyAuthor = lift . findOneUser . _replyAuthor
