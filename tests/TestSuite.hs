module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Text.Digestive.Validations.Parsers.Tests

main :: IO ()
main = defaultMain
    [ Text.Digestive.Validations.Parsers.Tests.localPhoneNumberTests
    , Text.Digestive.Validations.Parsers.Tests.phoneNumberPrecedenceTests
    , Text.Digestive.Validations.Parsers.Tests.phoneNumberWithAreaCodeTests
    , Text.Digestive.Validations.Parsers.Tests.phoneNumberWithAreaCodeAndCountryCodeTests
    ]