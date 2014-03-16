{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.PhoneNumber.Tests
    ( phoneNumberShowTests
    ) where

import Text.Digestive.Validations.Types.PhoneNumber
import Text.Digestive.Validations.Format(Format(..))
import Data.Monoid(mempty)
import Test.Framework                     (Test, testGroup)
import Test.Framework.Providers.HUnit     (testCase)
import Test.HUnit                         ((@=?))


phoneNumberShowTests :: Test
phoneNumberShowTests = testGroup "Text.Digestive.Validaionts.PhoneNumber: format"
    [ testCase "1" ("1" @=? (format $ mempty {_exchange = "1"}))

    , testCase "2" ("11" @=? (format $ mempty {_exchange = "11"}))

    , testCase "3" ("111" @=? (format $ mempty {_exchange = "111"}))

    , testCase "4" ("111 - 1" @=? (format $ mempty {_exchange = "111", _suffix="1"}))

    , testCase "5" ("111 - 1111" @=? (format $ mempty {_exchange = "111", _suffix="1111"}))

    , testCase "6" ("111 - 1111 x 11" @=? (format $ mempty {_exchange = "111", _suffix="1111", _extension = "11"}))

    , testCase "7" ("(111) 111 - 1111 x 11" @=? (format $ mempty {_areaCode = "111", _exchange = "111", _suffix="1111", _extension = "11"}))

    , testCase "7" ("1 (111) 111 - 1111 x 11" @=? (format $ mempty {_countryCode = "1", _areaCode = "111", _exchange = "111", _suffix="1111", _extension = "11"}))

    ]
