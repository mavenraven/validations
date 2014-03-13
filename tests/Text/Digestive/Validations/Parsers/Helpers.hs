module Text.Digestive.Validations.Parsers.Helpers
    ( testPhoneParser
    , testPhoneParserDoesNotParse
    ) where

import Text.Digestive.Validations.Parsers
import Text.Parsec.Prim (runParser)
import Data.Text(Text)

import Test.Framework                     (Test, testGroup)
import Test.Framework.Providers.HUnit     (testCase)
import Test.HUnit                         ((@=?), assertFailure, assertBool)



testPhoneParser = testParser runPhoneNumberParser
testParser parseRunner name input expected = testCase name $ case (parseRunner input) of
  Right x -> expected @=? x
  Left  e -> assertFailure (show e)

testPhoneParserDoesNotParse name input = testCase name $ case(runPhoneNumberParser input) of
  Left e -> assertBool "" True
  Right x -> assertFailure $ "input was incorrectly successfully parsed" ++ show x
