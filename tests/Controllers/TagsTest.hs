{-# LANGUAGE OverloadedStrings #-}


module Controllers.TagsTest (tests) where

import Data.Text (Text)
import Data.Bson (ObjectId)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert, assertFailure, (@?=))
import Data.Time
import System.IO.Unsafe (unsafePerformIO)

import Controllers.Tag
import Models.Tag

tests :: Test
tests = testGroup "Test.Controllers.Tag.saveTags"
    [ testCase "just empty if input empty" $ (@?=) (filterExistsTags [] []) []
    , testCase "just return if no-exists" $ (@?=) (filterExistsTags inputTags []) inputTags
    , testCase "just empty if input empyt" $ (@?=) (filterExistsTags [] exTags) []
    , testCase "got tag1 as only it is new" $ (@?=) (filterExistsTags inputTags exTags) newTag
    ]

-- | All replies which have no children replies
-- 
--testReplyWithoutChildren :: Assertion

--------------------------------------------------

-- Exists Tag
exTags :: [Tag]
exTags = map textToTag inputTagsText

textToTag :: Text -> Tag
textToTag name = emptyTag { _tagName = name }

inputTags = newTag ++ exTags

newTag = map textToTag newTagText

inputTagsText = ["tag3", "tag2"]

newTagText = ["tag1"]
