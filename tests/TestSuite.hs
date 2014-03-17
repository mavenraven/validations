module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Text.Digestive.Validations.Parsers.Tests
import qualified Text.Digestive.Validations.PhoneNumber.Tests

main :: IO ()
main = defaultMain
    [ Text.Digestive.Validations.Parsers.Tests.phoneNumberTests
    , Text.Digestive.Validations.Parsers.Tests.phoneNumberPrecedenceTests
    , Text.Digestive.Validations.PhoneNumber.Tests.phoneNumberShowTests
    ]
