module Views.Tests where

import Data.Text (Text)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert, assertFailure, (@=?))

import Views.ReplySplices

tests :: Test
tests = testGroup "Test.Views.ReplySplices"
    [ testCase "splitReplies empty" $ (@=?) (splitReplies []) []
    ]

---------------------------------------------------------
-- Test Datas


