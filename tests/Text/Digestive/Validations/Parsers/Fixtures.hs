{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.Parsers.Fixtures
    ( localPhoneNumber
    , localPhoneNumberWithExtension
    , phoneNumberWithAreaCode
    , phoneNumberWithAreaCodeAndExtension
    , phoneNumberWithAreaCodeAndCountryCode
    , phoneNumberWithAreaCodeAndExtendedCountryCode
    , phoneNumberWithAreaCodeAndCountryCodeAndExtension
    , phoneNumberWithAreaCodeAndCountryCodeAndOneDigitExtension
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
import Data.Monoid(mempty)

localPhoneNumber :: PhoneNumber
localPhoneNumber = mempty {_exchange = "781", _suffix = "4218" }

localPhoneNumberWithExtension :: PhoneNumber
localPhoneNumberWithExtension = mempty {_exchange = "781", _suffix = "4218", _extension = "1111"}

phoneNumberWithAreaCode :: PhoneNumber
phoneNumberWithAreaCode = mempty {_areaCode = "313", _exchange = "781", _suffix = "4218"}

phoneNumberWithAreaCodeAndCountryCode :: PhoneNumber
phoneNumberWithAreaCodeAndCountryCode = mempty {_countryCode = "1", _areaCode = "313", _exchange = "781", _suffix = "4218"}

phoneNumberWithAreaCodeAndExtendedCountryCode :: PhoneNumber
phoneNumberWithAreaCodeAndExtendedCountryCode = mempty {_countryCode = "1212", _areaCode = "313", _exchange = "781", _suffix = "4218"}

phoneNumberWithAreaCodeAndCountryCodeAndExtension :: PhoneNumber
phoneNumberWithAreaCodeAndCountryCodeAndExtension = mempty {_countryCode = "21", _areaCode = "313", _exchange = "781", _suffix = "4218", _extension="1111"}

phoneNumberWithAreaCodeAndCountryCodeAndOneDigitExtension :: PhoneNumber
phoneNumberWithAreaCodeAndCountryCodeAndOneDigitExtension = mempty {_countryCode = "1", _areaCode = "222", _exchange = "222", _suffix = "2222", _extension="1"}

phoneNumberWithAreaCodeAndExtension :: PhoneNumber
phoneNumberWithAreaCodeAndExtension = mempty {_areaCode = "313", _exchange = "781", _suffix = "4218", _extension = "1111"}

--oneDigitNumber :: PhoneNumber
--oneDigitNumber  = mempty {_exchange = "1", }

sevenDigitNumber :: PhoneNumber
sevenDigitNumber  = mempty {_exchange = "111", _suffix = "1111" }

eightDigitNumber :: PhoneNumber
eightDigitNumber  = mempty {_areaCode = "111", _exchange = "111", _suffix = "11"}

nineDigitNumber :: PhoneNumber
nineDigitNumber  = mempty {_areaCode = "111", _exchange = "111", _suffix = "111"}

tenDigitNumber :: PhoneNumber
tenDigitNumber  = mempty {_areaCode = "111", _exchange = "111", _suffix = "1111"}

elevenDigitNumber :: PhoneNumber
elevenDigitNumber = mempty {_countryCode = "1", _areaCode = "111", _exchange = "111", _suffix = "1111" }

twelveDigitNumber :: PhoneNumber
twelveDigitNumber = mempty {_countryCode = "11", _areaCode = "111", _exchange = "111", _suffix = "1111"}

thirteenDigitNumber :: PhoneNumber
thirteenDigitNumber = mempty {_countryCode = "111", _areaCode = "111", _exchange = "111", _suffix = "1111"}

fourteenDigitNumber :: PhoneNumber
fourteenDigitNumber = mempty {_countryCode = "1111", _areaCode = "111", _exchange = "111", _suffix = "1111"}

fifteenDigitNumber :: PhoneNumber
fifteenDigitNumber = mempty {_countryCode = "1111", _areaCode = "111", _exchange = "111", _suffix = "1111", _extension = "1"}
