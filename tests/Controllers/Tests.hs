{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tests (tests) where

import qualified Controllers.TagsTest (tests)
import           Test.Framework       (Test, testGroup)


tests :: Test
tests = testGroup "Test.Controllers"
    [
      Controllers.TagsTest.tests
    ]
