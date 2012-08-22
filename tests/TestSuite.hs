module Main where


import Test.Framework (defaultMain)

import qualified Views.Tests (tests)

main :: IO ()
main = defaultMain [ Views.Tests.tests ]
