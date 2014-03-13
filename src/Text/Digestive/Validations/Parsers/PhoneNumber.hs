{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.Parsers.PhoneNumber
    ( phoneNumberParser
    , runPhoneNumberParser
    )
    where

import Control.Applicative((<$>))
import Text.ParserCombinators.Parsec.Char 
import Text.Parsec.Prim 
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Text()
import Text.Parsec.Error (ParseError)
import Data.Text(Text, pack, cons)
import Text.Digestive.Validations.Types.PhoneNumber

type PhoneNumberParser m a = ParsecT Text PhoneNumber m a

betweenSpaces :: Monad m => ParsecT Text u m a -> ParsecT Text u m a
betweenSpaces = between spaces spaces

bracketedAreaCode :: Monad m => Char -> Char -> PhoneNumberParser m ()
bracketedAreaCode l r  = between (betweenSpaces (char l)) (betweenSpaces (char r)) areaCode

seperator :: Monad m => PhoneNumberParser m ()
seperator = spaces >> oneOf "-x" >> spaces

phoneDigits :: Monad m => Int -> (PhoneNumber -> Text -> PhoneNumber) -> PhoneNumberParser m ()
phoneDigits n f = betweenSpaces $ do
  ds <- pack <$> count n digit
  s <- getState
  putState (f s ds)

basicCountryCode :: Monad m => PhoneNumberParser m ()
basicCountryCode = try (cCode 3) <|>
                   try (cCode 2) <|>
                   try (cCode 1)
 where cCode n = phoneDigits n $ \s d -> s {_countryCode = d }

extendedCountryCode :: Monad m => PhoneNumberParser m ()
extendedCountryCode = betweenSpaces $ do
  d <- digit
  betweenSpaces (optional seperator)
  ds <- pack <$> count 3 digit 
  s <- getState
  putState s {_countryCode = cons d ds }

unbracketedAreaCode :: Monad m => PhoneNumberParser m ()
unbracketedAreaCode = phoneDigits 3 $ \s ds -> s {_areaCode = ds }

exchange  :: Monad m => PhoneNumberParser m ()
exchange = phoneDigits 3 $ \s ds -> s {_exchange = ds }

suffix :: Monad m => PhoneNumberParser m ()
suffix = phoneDigits 4 $ \s ds -> s {_suffix = ds }

extension :: Monad m => PhoneNumberParser m ()
extension = betweenSpaces $ do
  e <- pack <$> many1 digit
  s <- getState
  putState s {_extension = e }

countryCode :: Monad m => PhoneNumberParser m ()
countryCode = try extendedCountryCode <|> basicCountryCode

oneDigitCountryCode :: Monad m => PhoneNumberParser m ()
oneDigitCountryCode = phoneDigits 1 $ \s ds -> s {_countryCode = ds}

twoDigitCountryCode :: Monad m => PhoneNumberParser m ()
twoDigitCountryCode = phoneDigits 2 $ \s ds -> s {_countryCode = ds}

threeDigitCountryCode :: Monad m => PhoneNumberParser m ()
threeDigitCountryCode = phoneDigits 3 $ \s ds -> s {_countryCode = ds}

areaCode :: Monad m => PhoneNumberParser m ()
areaCode = try (bracketedAreaCode '(' ')') <|>
           try (bracketedAreaCode '[' ']') <|>
           try (bracketedAreaCode '{' '}') <|>
           unbracketedAreaCode


localPhoneNumber :: Monad m => PhoneNumberParser m ()
localPhoneNumber = betweenSpaces $ do
  exchange
  optional seperator
  suffix

phoneGrammar :: Monad m => PhoneNumberParser m ()
phoneGrammar = try (                                                                                 localPhoneNumber >> eof) <|>
               try (                                               areaCode >> optional seperator >> localPhoneNumber >> eof) <|>
               try (oneDigitCountryCode   >> optional seperator >> areaCode >> optional seperator >> localPhoneNumber >> eof) <|>
               try (twoDigitCountryCode   >> optional seperator >> areaCode >> optional seperator >> localPhoneNumber >> eof) <|>
               try (threeDigitCountryCode >> optional seperator >> areaCode >> optional seperator >> localPhoneNumber >> eof) <|>
               try (extendedCountryCode   >> optional seperator >> areaCode >> optional seperator >> localPhoneNumber >> eof) <|>

               try (countryCode >> optional seperator >> areaCode >> optional seperator >> localPhoneNumber >> optional seperator >> extension >> eof) <|>
               try (                                     areaCode >> optional seperator >> localPhoneNumber >> optional seperator >> extension >> eof) <|>
               try (                                                                       localPhoneNumber >> optional seperator >> extension >> eof) 


phoneNumberParser :: Monad m => PhoneNumberParser m PhoneNumber
phoneNumberParser = phoneGrammar >> getState

runPhoneNumberParser ::  Text -> Either ParseError PhoneNumber
runPhoneNumberParser input = runParser phoneNumberParser emptyPhoneNumber "" input
