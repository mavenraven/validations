module Validations.Parsers.Helpers
    ( testPhoneParser
    , testPhoneParserDoesNotParse
    ) where

import Validations.Parsers
import Data.Text(Text)
import Validations.Types.PhoneNumber(PhoneNumber)

import Test.Framework                     (Test, TestName)
import Test.Framework.Providers.HUnit     (testCase)
import Test.HUnit                         ((@=?), assertFailure, assertBool)




testPhoneParser :: TestName -> Text -> PhoneNumber -> Test
testPhoneParser = testParser runPhoneNumberParser

testParser :: (Show e) => (Text -> Either e PhoneNumber) -> TestName -> Text -> PhoneNumber -> Test
testParser parseRunner name input expected = testCase name $ case (parseRunner input) of
  Right x -> expected @=? x
  Left  e -> assertFailure (show e)

testPhoneParserDoesNotParse :: TestName -> Text -> Test
testPhoneParserDoesNotParse name input = testCase name $ case(runPhoneNumberParser input) of
  Left _ -> assertBool "" True
  Right x -> assertFailure $ "input was incorrectly successfully parsed" ++ show x
