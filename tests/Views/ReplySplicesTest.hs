{-# LANGUAGE OverloadedStrings #-}


module Views.ReplySplicesTest (tests) where

import           Data.Bson                      (ObjectId)
import           Data.Time
import           System.IO.Unsafe               (unsafePerformIO)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@?=))

import           Models.Reply
import           Views.ReplySplices

tests :: Test
tests = testGroup "Test.Views.ReplySplices.splitReplies"
    [ testCase "just empty" $ (@?=) (splitReplies []) []
    , testCase "reply without children" testReplyWithoutChildren
    , testCase "all children reply" testReplyAllChildren
    , testCase "reply with children" testReplyWithChildren
    ]

-- | All replies which have no children replies
--
testReplyWithoutChildren :: Assertion
testReplyWithoutChildren =
    let rs = justReply in
    (@?=) (splitReplies rs) (g rs)
    where g = map (\ r -> (r, []))


-- | All children replies
--
testReplyAllChildren :: Assertion
testReplyAllChildren =
    let rs = justChildrenReply in
    (@?=) (splitReplies rs) []

-- | reply and its children.
--
testReplyWithChildren :: Assertion
testReplyWithChildren =
    let rs = justReplyWithChildren in
    (@?=) (splitReplies rs) [ (reply1, [childrenReply11, childrenReply12])
                            , (reply2, [])
                            , (reply3, [childrenReply31]) ]

---------------------------------------------------------
-- Test Datas

justReply :: [Reply]
justReply = [reply1, reply2, reply3]

justChildrenReply :: [Reply]
justChildrenReply = [childrenReply11, childrenReply12, childrenReply31]

justReplyWithChildren :: [Reply]
justReplyWithChildren = justReply ++ justChildrenReply

reply1 :: Reply
reply1 = defaultReply  { _replyId = toObjId "12345678901"}

reply2 :: Reply
reply2 = defaultReply  { _replyId = toObjId "12345678902"}

reply3 :: Reply
reply3 = defaultReply  { _replyId = toObjId "12345678903"}

childrenReply11 :: Reply
childrenReply11 = defaultReply  { _replyToReplyId = toObjId "12345678901"}

childrenReply12 :: Reply
childrenReply12 = defaultReply  { _replyToReplyId = toObjId "12345678901"}

childrenReply31 :: Reply
childrenReply31 = defaultReply  { _replyToReplyId = toObjId "12345678903"}

defaultReply :: Reply
defaultReply = Reply
        { _replyId        = Nothing
        , _replyToTopicId = read "1234567890"
        , _replyToReplyId = Nothing
        , _replyContent   = "Dumy comment"
        , _replyAuthor    = read "1234567891"
        , _replyCreateAt  = mockUTCTime
        }

mockUTCTime :: UTCTime
{-# NOINLINE mockUTCTime #-}
mockUTCTime = unsafePerformIO getCurrentTime

toObjId :: String -> Maybe ObjectId
toObjId = Just . read
