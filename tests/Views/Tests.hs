{-# LANGUAGE OverloadedStrings #-}


module Views.Tests (tests) where

import Test.Framework (Test, testGroup)
import qualified Views.ReplySplicesTest (tests)
import qualified Views.PaginationSplicesTest (tests)


tests :: Test
tests = testGroup "Test.Views"
    [ Views.ReplySplicesTest.tests
    , Views.PaginationSplicesTest.tests
    ]
