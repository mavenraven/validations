module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Validations.Parsers.Tests
import qualified Validations.Checkers.PhoneNumber.Tests

main :: IO ()
main = defaultMain
    [ Validations.Parsers.Tests.phoneNumberTests
    , Validations.Parsers.Tests.phoneNumberPrecedenceTests

    , Validations.Checkers.PhoneNumber.Tests.phoneNumberShowTests
    ]
