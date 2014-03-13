{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.PhoneNumber
  ( phoneNumber
  , hasAreaCode
  , hasExtension
  , hasCountryCode
  ) 
    where

import Text.Digestive.Validations.Parsers.PhoneNumber
import Text.Digestive.Validations.Types.PhoneNumber
import Text.Digestive.Validations.Localization
import Text.Digestive.Types(Result(..))
import Data.Text(unpack)

phoneNumber txt = case(runPhoneNumberParser txt) of
  Right x -> Success x
  Left  _ -> Error $ message
    [ LocalizedMessage english Nothing "Phone number is not valid."
    , LocalizedMessage dutch   Nothing "<insert dutch error sentence>."
    ]


hasPhoneField accessor msg ph = case((null . unpack . accessor) ph) of
  False -> Success ph
  True  -> Error $ message msg

hasAreaCode = hasPhoneField _areaCode  
  [ LocalizedMessage english (Just "should have an area code") "Phone number should have an area code."
  ]

hasExtension = hasPhoneField _extension
  [ LocalizedMessage english (Just "should have an extension") "Phone number should have an extension."
  ]

hasCountryCode = hasPhoneField _countryCode
  [ LocalizedMessage english (Just "should have a country code") "Phone number should have a country code."
  ]
