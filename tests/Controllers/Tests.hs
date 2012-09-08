{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tests (tests) where

import Test.Framework (Test, testGroup)
import qualified Controllers.TagsTest (tests)


tests :: Test
tests = testGroup "Test.Controllers"
    [ 
      Controllers.TagsTest.tests
    ]
