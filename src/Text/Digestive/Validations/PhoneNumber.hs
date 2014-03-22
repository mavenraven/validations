{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.PhoneNumber
  ( phoneNumber
  , hasAreaCode
  , hasExtension
  , hasCountryCode
  ) 
    where

import Prelude hiding (null)
import Text.Digestive.Validations.Parsers.PhoneNumber(runPhoneNumberParser)
import Text.Digestive.Validations.Types.PhoneNumber(PhoneNumber(..))
import Text.Digestive.Validations.Localization(LocalizedMessage, french, english, dutch, Message(..), message)
import Data.Text(null, Text)


phoneNumber :: Text -> Either LocalizedMessage PhoneNumber
phoneNumber txt = case(runPhoneNumberParser txt) of
  Right x -> Right x
  Left  _ -> Left $ message
    [ Message english Nothing "Phone number is not valid."
    , Message dutch   Nothing "<insert dutch error sentence>."
    , Message french  Nothing "<insert french error sentence>."
    ]


hasPhoneField :: (PhoneNumber -> Text) -> [Message] -> PhoneNumber -> Either LocalizedMessage PhoneNumber
hasPhoneField accessor msg ph = case((null . accessor) ph) of
  False -> Right ph
  True  -> Left $ message msg

hasAreaCode :: PhoneNumber -> Either LocalizedMessage PhoneNumber
hasAreaCode = hasPhoneField _areaCode  
  [ Message english (Just "should have an area code") "Phone number should have an area code."
  ]

hasExtension :: PhoneNumber -> Either LocalizedMessage PhoneNumber
hasExtension = hasPhoneField _extension
  [ Message english (Just "should have an extension") "Phone number should have an extension."
  ]

hasCountryCode :: PhoneNumber -> Either LocalizedMessage PhoneNumber
hasCountryCode = hasPhoneField _countryCode
  [ Message english (Just "should have a country code") "Phone number should have a country code."
  ]
