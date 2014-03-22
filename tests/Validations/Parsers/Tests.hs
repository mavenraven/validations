{-# LANGUAGE OverloadedStrings #-}

module Validations.Parsers.Tests
    ( phoneNumberTests
    , phoneNumberPrecedenceTests
    ) where

import qualified Validations.Parsers.Fixtures as F
import Validations.Parsers.Helpers (testPhoneParser, testPhoneParserDoesNotParse)
import Test.Framework                     (Test, testGroup)
import Data.Monoid(mempty)
import Validations.Types.PhoneNumber(PhoneNumber(..))


phoneNumberPrecedenceTests :: Test
phoneNumberPrecedenceTests = testGroup "Text.Digestive.Validations.Parsers.PhoneNumber: digit precedence"
    [ testPhoneParser "1" "1111111" F.sevenDigitNumber

    , testPhoneParser "2" "11111111" F.eightDigitNumber

    , testPhoneParser "3" "111111111" F.nineDigitNumber

    , testPhoneParser "4" "1111111111" F.tenDigitNumber

    , testPhoneParser "5" "11111111111" F.elevenDigitNumber

    , testPhoneParser "6" "111111111111" F.twelveDigitNumber

    , testPhoneParser "7" "1111111111111" F.thirteenDigitNumber

    , testPhoneParser "8" "11111111111111" F.fourteenDigitNumber

    , testPhoneParser "9" "111111111111111" F.fifteenDigitNumber
    ]

phoneNumberTests :: Test
phoneNumberTests = testGroup "Text.Digestive.Validations.Parsers.PhoneNumber "
    [ testPhoneParser "1" "781-4218" F.localPhoneNumber

    , testPhoneParser "2" "781 4218" F.localPhoneNumber

    , testPhoneParser "3" "7814218" F.localPhoneNumber

    , testPhoneParser "4" "781    4218" F.localPhoneNumber
    
    , testPhoneParser "5" "781-4218x1111" F.localPhoneNumberWithExtension

    , testPhoneParser "6" "(313)781-4218" F.phoneNumberWithAreaCode

    , testPhoneParser "7" "(313)781 4218" F.phoneNumberWithAreaCode

    , testPhoneParser "8" "(313) - 781 4218" F.phoneNumberWithAreaCode

    , testPhoneParser "9" "(313)7814218" F.phoneNumberWithAreaCode

    , testPhoneParser "10" "3137814218" F.phoneNumberWithAreaCode

    , testPhoneParser "11" "(313)781    4218" F.phoneNumberWithAreaCode

    , testPhoneParser "12" "313 781    4218" F.phoneNumberWithAreaCode

    , testPhoneParser "13" "[313] 781 4218" F.phoneNumberWithAreaCode

    , testPhoneParser "14" "{313} 781 4218" F.phoneNumberWithAreaCode

    , testPhoneParserDoesNotParse "15" "(313 781 421"
    
    , testPhoneParser "16" "313 781-4218x1111" F.phoneNumberWithAreaCodeAndExtension

    , testPhoneParser "17" "[313]   781 4218 1111" F.phoneNumberWithAreaCodeAndExtension

    , testPhoneParser "18" "1-313-781-4218" F.phoneNumberWithAreaCodeAndCountryCode

    , testPhoneParser "19" "1 313 781 4218" F.phoneNumberWithAreaCodeAndCountryCode

    , testPhoneParser "20" "1 212 (313) - 781 4218" F.phoneNumberWithAreaCodeAndExtendedCountryCode

    , testPhoneParser "21" "21 313 781-4218x1111" F.phoneNumberWithAreaCodeAndCountryCodeAndExtension

    , testPhoneParser "22" "1{" (mempty {_countryCode = "1" })

    , testPhoneParser "23" "1{234" (mempty {_countryCode = "1" , _areaCode = "234" })

    , testPhoneParser "24" "1{234}" (mempty {_countryCode = "1" , _areaCode = "234" })

    , testPhoneParser "25" "1 - {234}" (mempty {_countryCode = "1" , _areaCode = "234" })

    , testPhoneParser "26" "12 - {234}" (mempty {_countryCode = "12" , _areaCode = "234" })

    , testPhoneParser "27" "123 - {234}" (mempty {_countryCode = "123" , _areaCode = "234" })

    , testPhoneParser "28" "1234 - {234}" (mempty {_countryCode = "1234" , _areaCode = "234" })

    , testPhoneParserDoesNotParse "29" "1234 - {234} 22-2" 

    , testPhoneParserDoesNotParse "30" "1234 - {234} 222-222-2" 

    , testPhoneParserDoesNotParse "31" "111-111x11" 

    , testPhoneParser "32" "111-1111x11" (mempty {_exchange = "111", _suffix = "1111", _extension = "11"})

    , testPhoneParser "33" "111-1111-11" (mempty {_exchange = "111", _suffix = "1111", _extension = "11"})

    , testPhoneParser "34" "11-111-111" (mempty {_countryCode = "11", _areaCode = "111", _exchange = "111"})

    , testPhoneParser "35" "11-111-111-1111-11" (mempty {_countryCode = "11", _areaCode = "111", _exchange = "111", _suffix = "1111", _extension = "11"})

    , testPhoneParserDoesNotParse "36" "1{24}" 

    , testPhoneParser "37" "1 - {234]" (mempty {_countryCode = "1" , _areaCode = "234" })

    , testPhoneParserDoesNotParse "38" "1 - {2344]4" 

    ] 
