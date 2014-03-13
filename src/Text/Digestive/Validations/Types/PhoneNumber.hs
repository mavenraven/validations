{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.Types.PhoneNumber
    ( PhoneNumber(..)
    , emptyPhoneNumber
    )
    where

import Data.Text(Text)

data PhoneNumber = PhoneNumber
  { _countryCode :: Text
  , _areaCode    :: Text
  , _exchange    :: Text
  , _suffix      :: Text
  , _extension   :: Text
  }
  deriving (Show, Eq)

emptyPhoneNumber :: PhoneNumber
emptyPhoneNumber = PhoneNumber
  { _countryCode = ""
  , _areaCode = ""
  , _exchange = ""
  , _suffix   = ""
  , _extension = ""
  }
