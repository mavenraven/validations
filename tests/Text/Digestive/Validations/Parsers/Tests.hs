{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.Parsers.Tests
    ( localPhoneNumberTests
    , phoneNumberPrecedenceTests
    , phoneNumberWithAreaCodeTests
    , phoneNumberWithAreaCodeAndCountryCodeTests
    ) where

import Text.Digestive.Validations.Parsers
import qualified Text.Digestive.Validations.Parsers.Fixtures as F
import Text.Digestive.Validations.Parsers.Helpers (testPhoneParser, testPhoneParserDoesNotParse)
import Test.Framework (defaultMain)
import Test.Framework                     (Test, testGroup)
import Test.Framework.Providers.HUnit     (testCase)
import Test.HUnit                         ((@=?))


phoneNumberPrecedenceTests :: Test
phoneNumberPrecedenceTests = testGroup "Text.Digestive.Parsers.PhoneNumber: digit precedence"
    [ testPhoneParser "seven digit number" "1111111" F.sevenDigitNumber

    , testPhoneParser "eight digit number" "11111111" F.eightDigitNumber

    , testPhoneParser "nine digit number" "111111111" F.nineDigitNumber

    , testPhoneParser "ten digit number" "1111111111" F.tenDigitNumber

    , testPhoneParser "eleven digit number" "11111111111" F.elevenDigitNumber

    , testPhoneParser "twelve digit number" "111111111111" F.twelveDigitNumber

    , testPhoneParser "thirteen digit number" "1111111111111" F.thirteenDigitNumber

    , testPhoneParser "fourteen digit number" "11111111111111" F.fourteenDigitNumber

    , testPhoneParser "fifteen digit number" "111111111111111" F.fifteenDigitNumber
    ]

localPhoneNumberTests :: Test
localPhoneNumberTests = testGroup "Text.Digestive.Parsers.PhoneNumber: local number"
    [ testPhoneParser "simplest case" "781-4218" F.localPhoneNumber

    , testPhoneParser "no seperator" "781 4218" F.localPhoneNumber

    , testPhoneParser "no space" "7814218" F.localPhoneNumber

    , testPhoneParser "number of spaces between has no effect" "781    4218" F.localPhoneNumber

    , testPhoneParserDoesNotParse "does not parse only 6 digits" "781 421"
    
    , testPhoneParser "can parse with extension" "781-4218x1111" F.localPhoneNumberWithExtension

    , testPhoneParser "can parse with extension no seperator" "781 4218 1111" F.localPhoneNumberWithExtension


    ]

phoneNumberWithAreaCodeTests :: Test
phoneNumberWithAreaCodeTests = testGroup "Text.Digestive.Parsers.PhoneNumber: phone number with area code"
    [ testPhoneParser "simplest case" "(313)781-4218" F.phoneNumberWithAreaCode

    , testPhoneParser "no seperator" "(313)781 4218" F.phoneNumberWithAreaCode

    , testPhoneParser "two seperators" "(313) - 781 4218" F.phoneNumberWithAreaCode

    , testPhoneParser "no space" "(313)7814218" F.phoneNumberWithAreaCode

    , testPhoneParser "no space and no surrounders" "3137814218" F.phoneNumberWithAreaCode

    , testPhoneParser "number of spaces between has no effect" "(313)781    4218" F.phoneNumberWithAreaCode

    , testPhoneParser "does not need to be surrounded" "313 781    4218" F.phoneNumberWithAreaCode

    , testPhoneParser "can use [ ]" "[313] 781 4218" F.phoneNumberWithAreaCode

    , testPhoneParser "can use { }" "{313} 781 4218" F.phoneNumberWithAreaCode

    , testPhoneParserDoesNotParse "surrounderes must be balanced" "(313 781 421"
    
    , testPhoneParser "can parse with extension" "313 781-4218x1111" F.phoneNumberWithAreaCodeAndExtension

    , testPhoneParser "can parse with extension no seperator" "[313]   781 4218 1111" F.phoneNumberWithAreaCodeAndExtension
    ]

phoneNumberWithAreaCodeAndCountryCodeTests :: Test
phoneNumberWithAreaCodeAndCountryCodeTests = testGroup "Text.Digestive.Parsers.PhoneNumber: phone number with area code and country code"
    [ testPhoneParser "simplest case" "1-313-781-4218" F.phoneNumberWithAreaCodeAndCountryCode

    , testPhoneParser "no seperator" "1 313 781 4218" F.phoneNumberWithAreaCodeAndCountryCode

    , testPhoneParser "extended country code" "1 212 (313) - 781 4218" F.phoneNumberWithAreaCodeAndExtendedCountryCode

    , testPhoneParser "can parse with extension" "21 313 781-4218x1111" F.phoneNumberWithAreaCodeAndCountryCodeAndExtension
    ] 
