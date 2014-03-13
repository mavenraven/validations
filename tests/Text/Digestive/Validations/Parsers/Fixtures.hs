{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.Parsers.Fixtures
    ( localPhoneNumber
    , localPhoneNumberWithExtension
    , phoneNumberWithAreaCode
    , phoneNumberWithAreaCodeAndExtension
    , phoneNumberWithAreaCodeAndCountryCode
    , phoneNumberWithAreaCodeAndExtendedCountryCode
    , phoneNumberWithAreaCodeAndCountryCodeAndExtension
    , sevenDigitNumber
    , eightDigitNumber
    , nineDigitNumber
    , tenDigitNumber
    , elevenDigitNumber
    , twelveDigitNumber
    , thirteenDigitNumber
    , fourteenDigitNumber
    , fifteenDigitNumber
    ) where

import Text.Digestive.Validations.Types

localPhoneNumber :: PhoneNumber
localPhoneNumber = emptyPhoneNumber {_exchange = "781", _suffix = "4218" }

localPhoneNumberWithExtension :: PhoneNumber
localPhoneNumberWithExtension = emptyPhoneNumber {_exchange = "781", _suffix = "4218", _extension = "1111"}

phoneNumberWithAreaCode :: PhoneNumber
phoneNumberWithAreaCode = emptyPhoneNumber {_areaCode = "313", _exchange = "781", _suffix = "4218"}

phoneNumberWithAreaCodeAndCountryCode :: PhoneNumber
phoneNumberWithAreaCodeAndCountryCode = emptyPhoneNumber {_countryCode = "1", _areaCode = "313", _exchange = "781", _suffix = "4218"}

phoneNumberWithAreaCodeAndExtendedCountryCode :: PhoneNumber
phoneNumberWithAreaCodeAndExtendedCountryCode = emptyPhoneNumber {_countryCode = "1212", _areaCode = "313", _exchange = "781", _suffix = "4218"}

phoneNumberWithAreaCodeAndCountryCodeAndExtension :: PhoneNumber
phoneNumberWithAreaCodeAndCountryCodeAndExtension = emptyPhoneNumber {_countryCode = "21", _areaCode = "313", _exchange = "781", _suffix = "4218", _extension="1111"}

phoneNumberWithAreaCodeAndExtension :: PhoneNumber
phoneNumberWithAreaCodeAndExtension = emptyPhoneNumber {_areaCode = "313", _exchange = "781", _suffix = "4218", _extension = "1111"}

sevenDigitNumber :: PhoneNumber
sevenDigitNumber  = emptyPhoneNumber {_exchange = "111", _suffix = "1111" }

eightDigitNumber :: PhoneNumber
eightDigitNumber  = emptyPhoneNumber {_exchange = "111", _suffix = "1111", _extension = "1" }

nineDigitNumber :: PhoneNumber
nineDigitNumber  = emptyPhoneNumber {_exchange = "111", _suffix = "1111", _extension = "11" }

tenDigitNumber :: PhoneNumber
tenDigitNumber  = emptyPhoneNumber {_areaCode = "111", _exchange = "111", _suffix = "1111"}

elevenDigitNumber :: PhoneNumber
elevenDigitNumber = emptyPhoneNumber {_countryCode = "1", _areaCode = "111", _exchange = "111", _suffix = "1111" }

twelveDigitNumber :: PhoneNumber
twelveDigitNumber = emptyPhoneNumber {_countryCode = "11", _areaCode = "111", _exchange = "111", _suffix = "1111"}

thirteenDigitNumber :: PhoneNumber
thirteenDigitNumber = emptyPhoneNumber {_countryCode = "111", _areaCode = "111", _exchange = "111", _suffix = "1111"}

fourteenDigitNumber :: PhoneNumber
fourteenDigitNumber = emptyPhoneNumber {_countryCode = "1111", _areaCode = "111", _exchange = "111", _suffix = "1111"}

fifteenDigitNumber :: PhoneNumber
fifteenDigitNumber = emptyPhoneNumber {_countryCode = "1111", _areaCode = "111", _exchange = "111", _suffix = "1111", _extension = "1"}
