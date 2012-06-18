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
    [ testCase "Nothing to 1" $ (@?=) (forceNonEmpty Nothing) 1
    , testCase "Just 0 to 1" $ (@?=) (forceNonEmpty (Just 0)) 1
    , testCase "Just x to x" $ (@?=) (forceNonEmpty (Just 2)) 2
    , testCase "page list: 0 0" $ (@?=) (genPaginationList 0 0) []
    , testCase "page list: 2 0" $ (@?=) (genPaginationList 2 0) []
    , testCase "page list: 0 4" $ (@?=) (genPaginationList 0 4) [1,2]
    , testCase "page list: 2 4" $ (@?=) (genPaginationList 2 4) [1,2]      
    , testCase "node: 0 0" $ (@?=) verifyEmptyUL True
    , testCase "node: 0 4" $ (@?=) verifyULNode True
    ]

-- | FIXME: verification method need improve.
-- 
verifyEmptyUL = verifyNodeGen 0 0 (\ node _ _ -> X.isComment node)


verifyULNode = isNodeUL 0 4 (\ _ _ lis -> not $ null lis)


isNodeUL x y f = verifyNodeGen x y g
               where g node name lis = isJust name 
                                       && "ul" == fromJust name
                                       && f node name lis

verifyNodeGen x y f = let node = paginationNode x y
                          tName = X.tagName node
                          lis  = X.childNodes node in
                      f node tName lis


---------------------------------------------------------
-- Test Datas

