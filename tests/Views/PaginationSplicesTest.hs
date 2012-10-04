{-# LANGUAGE OverloadedStrings #-}

module Views.PaginationSplicesTest (tests) where

import           Data.Maybe
import           Data.Text                      (Text)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@?=))
import qualified Text.XmlHtml                   as X

import           Views.PaginationSplices

tests :: Test
tests = testGroup "Test.Views.PaginationSplices"
    [
      testCase "node: 0 0" $ (@?=) verifyEmptyUL True
    , testCase "node: 0 [1,2]" $ (@?=) (verifyULNode 1 [1,2]) True
    , testCase "slice: 1 0 [1,2,3]" $ (@?=) (sliceForPage 3 0 [1,2,3]) []
    , testCase "slice: 33 2 [1,2,3]" $ (@?=) (sliceForPage 3 pageSize [1,2,3]) []
    , testCase "slice: 0 2 [1,2,3,4]" $ (@?=) (sliceForPage 0 pageSize [1,2,3]) [1,2]
    , testCase "slice: 1 2 [1,2,3,4]" $ (@?=) (sliceForPage 1 pageSize [1,2,3,4]) [1,2]
    , testCase "slice: 2 2 [1,2,3,4]" $ (@?=) (sliceForPage 2 pageSize [1,2,3,4]) [3,4]
    ]

-- | FIXME: verification method need improve.
--

verifyEmptyUL = f (nodeDataGen 0 [])
                where f (node, _,  _) = X.isComment node


verifyULNode x y = f (nodeDataGen x y)
               where f (_, name, lis) = isNodeUL name
                                        && not (null lis)
                                        && length lis == length y

isNodeUL name  = isJust name && "ul" == fromJust name

nodeDataGen x y  = let node  = paginationNode x y urlGenRoot
                       tName = X.tagName node
                       lis   = X.childNodes node
                   in (node, tName, lis)

pageSize :: Int
pageSize = 2

urlGenRoot :: Text -> Text
urlGenRoot = urlGen "/"

---------------------------------------------------------
-- Test Datas
