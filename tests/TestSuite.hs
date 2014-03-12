{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Text.Digestive.Validations

import Test.Framework (defaultMain)
import           Test.Framework                     (Test, testGroup)
import           Test.Framework.Providers.HUnit     (testCase)
import           Test.HUnit                         ((@=?))


main :: IO ()
main = defaultMain
    [ initTests
    ]

initTests :: Test
initTests = testGroup "Text.Digestive.Validations"
    [ testCase "f" $
        True @=? f

    , testCase "g" $
        False @=? g

    ]

