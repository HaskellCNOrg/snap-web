{-# LANGUAGE OverloadedStrings #-}


module Views.PaginationSplicesTest (tests) where

import Data.Text (Text)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert, assertFailure, (@?=))
import qualified Text.XmlHtml as X
import Data.Maybe 

import Views.PaginationSplices

tests :: Test
tests = testGroup "Test.Views.PaginationSplices"
    [ testCase "page list: 0" $ (@?=) (genPaginationList 0) []
    , testCase "page list: 1" $ (@?=) (genPaginationList 1) [1]
    , testCase "page list: 4" $ (@?=) (genPaginationList 4) [1,2]
    , testCase "node: 0 0" $ (@?=) verifyEmptyUL True
    , testCase "node: 0 4" $ (@?=) (verifyULNode 0 4) True
    , testCase "slice: 0 []" $ (@?=) (sliceForPage 0 [""]) [""]
    , testCase "slice: 0 [1,2,3,4]" $ (@?=) (sliceForPage 0 [1,2,3,4]) [1,2]
    , testCase "slice: 1 [1,2,3,4]" $ (@?=) (sliceForPage 1 [1,2,3,4]) [1,2]
    , testCase "slice: 2 [1,2,3,4]" $ (@?=) (sliceForPage 2 [1,2,3,4]) [3,4]
    ]

-- | FIXME: verification method need improve.
-- 
verifyEmptyUL = verifyNodeGen 0 
                (genPaginationList 0)
                (\ node _ _ -> X.isComment node)


verifyULNode x y = isNodeUL x
                   (genPaginationList y)
                   (\ _ _ lis -> not (null lis) && (length lis == pageCount y))


isNodeUL x y f = verifyNodeGen x y 
                 (\ node name lis -> isJust name && "ul" == fromJust name 
                                     && f node name lis)


verifyNodeGen x y f = let node  = paginationNode x y
                          tName = X.tagName node
                          lis   = X.childNodes node
                      in f node tName lis

pageCount n = ceiling $ n / pageSize

---------------------------------------------------------
-- Test Datas

