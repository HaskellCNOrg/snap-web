{-# LANGUAGE OverloadedStrings #-}


module Views.Tests (tests) where

import           Test.Framework              (Test, testGroup)
import qualified Views.PaginationSplicesTest (tests)
import qualified Views.ReplySplicesTest      (tests)


tests :: Test
tests = testGroup "Test.Views"
    [ Views.ReplySplicesTest.tests
    , Views.PaginationSplicesTest.tests
    ]
