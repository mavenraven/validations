{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.Types.PhoneNumber
    ( PhoneNumber(..)
    )
    where

import Prelude hiding (null)
import Data.Text(Text, null)
import Data.Monoid(Monoid(mempty, mappend), (<>))
import Control.Monad.State(get, put)
import Control.Monad.State.Strict(execState)
import Text.Digestive.Validations.Format(Format(..))

data PhoneNumber = PhoneNumber
  { _countryCode :: Text
  , _areaCode    :: Text
  , _exchange    :: Text
  , _suffix      :: Text
  , _extension   :: Text
  }
  deriving (Show, Eq)


isEmpty :: (PhoneNumber -> Text) -> PhoneNumber -> Bool
isEmpty accessor ph = (null . accessor) ph


data PhoneNumberState =
    Exchange PhoneNumber
  | ExchangeSuffix PhoneNumber
  | ExchangeSuffixExtension PhoneNumber
  | AreaCode PhoneNumber
  | AreaCodeExchange PhoneNumber
  | AreaCodeExchangeSuffix PhoneNumber
  | AreaCodeExchangeSuffixExtension PhoneNumber
  | CountryCode PhoneNumber
  | CountryCodeAreaCode PhoneNumber
  | CountryCodeAreaCodeExchange PhoneNumber
  | CountryCodeAreaCodeExchangeSuffix PhoneNumber
  | CountryCodeAreaCodeExchangeSuffixExtension PhoneNumber
  | Invalid PhoneNumber

phoneNumberState :: PhoneNumber -> PhoneNumberState 
phoneNumberState ph = state (countryCodeEmpty, areaCodeEmpty, exchangeEmpty, suffixEmpty, extensionEmpty)
  where state (True,True,False,True,True)     = Exchange ph
        state (True,True,False,False,True)    = ExchangeSuffix ph
        state (True,True,False,False,False)   = ExchangeSuffixExtension ph
        state (True,False,True,True,True)     = AreaCode ph
        state (True,False,False,True,True)    = AreaCodeExchange ph
        state (True,False,False,False,True)   = AreaCodeExchangeSuffix ph
        state (True,False,False,False,False)  = AreaCodeExchangeSuffixExtension ph
        state (False,True,True,True,True)     = CountryCode ph
        state (False,False,True,True,True)    = CountryCodeAreaCode ph
        state (False,False,False,True,True)   = CountryCodeAreaCodeExchange ph
        state (False,False,False,False,True)  = CountryCodeAreaCodeExchangeSuffix ph
        state (False,False,False,False,False) = CountryCodeAreaCodeExchangeSuffixExtension ph
        state (_,_,_,_,_) = Invalid ph
        countryCodeEmpty = isEmpty _countryCode ph
        areaCodeEmpty    = isEmpty _areaCode ph
        exchangeEmpty    = isEmpty _exchange ph
        suffixEmpty      = isEmpty _suffix ph
        extensionEmpty   = isEmpty _extension ph


instance Format PhoneNumber where
  format ph = case (phoneNumberState ph) of
    (Exchange x) -> _exchange x

    (ExchangeSuffix x) ->
      _exchange x <> 
            " - " <>
        _suffix x

    (ExchangeSuffixExtension x) ->
      _exchange x <>
            " - " <>
        _suffix x <> 
            " x " <>
     _extension x

    (AreaCode x) ->
              "(" <>
      _areaCode x <>
              ")"

    (AreaCodeExchange x) ->
              "(" <>
      _areaCode x <>
             ") " <>
      _exchange x

    (AreaCodeExchangeSuffix x) ->        
              "(" <>
      _areaCode x <>
             ") " <>
      _exchange x <>
           " - "  <>
        _suffix x

    (AreaCodeExchangeSuffixExtension x) ->
              "(" <>
      _areaCode x <>
             ") " <>
      _exchange x <>
           " - "  <>
        _suffix x <>
           " x "  <>
     _extension ph

    (CountryCode x) -> _countryCode x

    (CountryCodeAreaCode x) ->
      _countryCode x <>
                 " (" <>
         _areaCode x <>
                ") "

    (CountryCodeAreaCodeExchange x) ->
      _countryCode x <>
                 " (" <>
         _areaCode x <>
                ") " <>
         _exchange x

    (CountryCodeAreaCodeExchangeSuffix x) ->
      _countryCode x <>
                " (" <>
         _areaCode x <>
                ") " <>
         _exchange x <>
              " - "  <>
           _suffix x

    (CountryCodeAreaCodeExchangeSuffixExtension x) ->
      _countryCode x <>
                " (" <>
         _areaCode x <>
                ") " <>
         _exchange x <>
              " - "  <>
           _suffix x <>
              " x "  <>
        _extension x

    (Invalid x) -> 
      _countryCode x <>
         _areaCode x <>
         _exchange x <>
           _suffix x <>
        _extension x


instance Monoid PhoneNumber where
  mempty = PhoneNumber
    { _countryCode = ""
    , _areaCode = ""
    , _exchange = ""
    , _suffix   = ""
    , _extension = ""
    }

  mappend x y = (flip execState) x $ do

    _ph <- get
    if isEmpty _countryCode _ph
      then put $ _ph {_countryCode = (_countryCode y)}
      else return ()

    _ph <- get
    if isEmpty _areaCode _ph
      then put $ _ph {_areaCode = (_areaCode y)}
      else return ()

    _ph <- get
    if isEmpty _exchange _ph
      then put $ _ph {_exchange = (_exchange y)}
      else return ()

    _ph <- get
    if isEmpty  _suffix _ph
      then put $ _ph {_suffix = (_suffix y)}
      else return ()

    _ph <- get
    if isEmpty _extension _ph
      then put $ _ph {_suffix = (_extension y)}
      else return ()


