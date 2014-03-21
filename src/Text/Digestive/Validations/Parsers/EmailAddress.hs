{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.Parsers.PhoneNumber
    ( emailAddressParser
    , runEmailAddressParser
    , tokenizeEmailAddress
    , EmailAddressParser
    )
    where

import Prelude hiding (filter, any, length, min, max)
import Text.ParserCombinators.Parsec.Char (oneOf, noneOf)
import Text.Parsec.Prim (Stream, getState, putState, ParsecT, runParser, try, (<|>), many)
import Text.ParserCombinators.Parsec.Combinator(count, many1, manyTill, eof, choice, between)
import Text.Parsec.Text()
import Text.Parsec.Error (ParseError)
import Data.Text(Text, singleton, filter)
import Data.Monoid(mempty, (<>))
import Text.Digestive.Validations.Types.PhoneNumber(PhoneNumber(..))


type PhoneNumberParser m a = ParsecT Text (PhoneNumber) m a

--rfc 5322, http://tools.ietf.org/html/rfc5322



alpha = ['a' .. 'z'] <> ['A' .. 'Z']
digit = ['0' .. '9'] 

symbol = "!" <> "#" <>
         "$" <> "%" <>
         "&" <> "'" <>
         "*" <> "+" <>
         "_" <> "/" <>
         "=" <> "?" <>
         "^" <> "_" <>
         "`" <> "{" <>
         "|" <> "}" <>
         "~"

atToken = '@'

atext = oneOf (alpha <> digit <> symbol)

CFWS = " " <> "\t" <> "\n"

atom = many (oneOf CFWS) >> many1 atext >> many (oneOf CFWS)

dot-atom-text = many1k



atext = try (oneOf alphaNum) <|> try (oneOf symbol)






addr-spec = local-part >> (char atToken) >> domain

extensionSeperator :: String
extensionSeperator = "x"

dash :: String
dash = "-"

tokenizePhoneNumber :: Text -> Text
tokenizePhoneNumber = filter (`elem` digit <> lbrace <> extensionSeperator <> dash <> rbrace)

phoneNumberParser :: Monad m => PhoneNumberParser m PhoneNumber
phoneNumberParser = phoneGrammar >> getState

runPhoneNumberParser ::  Text -> Either ParseError PhoneNumber
runPhoneNumberParser input = runParser phoneNumberParser (mempty :: PhoneNumber ) "" (tokenizePhoneNumber input)

phoneGrammar :: Monad m => PhoneNumberParser m ()
phoneGrammar = 
               try (countryCode >> openingAreaCode >> noMoreImportantTokens) <|>
               try (openingAreaCode >> noMoreImportantTokens) <|>
  
               try (exchange >> noMoreImportantTokens) <|>
               try (localNumber >> noMoreImportantTokens) <|>
               try (fullLocalNumber >> qualifiedExtension >> noMoreImportantTokens) <|>

               try (areaCode >> exchange >> noMoreImportantTokens) <|>
               try (areaCode >> localNumber >> noMoreImportantTokens) <|>
               try (areaCode >> fullLocalNumber >> qualifiedExtension >> noMoreImportantTokens) <|>

               try (oneDigitCountryCode >> areaCode >> exchange >> noMoreImportantTokens) <|>
               try (oneDigitCountryCode >> areaCode >> localNumber >> noMoreImportantTokens) <|>
               try (oneDigitCountryCode >> areaCode >> fullLocalNumber >> qualifiedExtension >> noMoreImportantTokens) <|>

               try (twoDigitCountryCode >> areaCode >> exchange >> noMoreImportantTokens) <|>
               try (twoDigitCountryCode >> areaCode >> localNumber >> noMoreImportantTokens) <|>
               try (twoDigitCountryCode >> areaCode >> fullLocalNumber >> qualifiedExtension >> noMoreImportantTokens) <|>

               try (threeDigitCountryCode >> areaCode >> exchange >> noMoreImportantTokens) <|>
               try (threeDigitCountryCode >> areaCode >> localNumber >> noMoreImportantTokens) <|>
               try (threeDigitCountryCode >> areaCode >> fullLocalNumber >> qualifiedExtension >> noMoreImportantTokens) <|>

               try (fourDigitCountryCode >> areaCode >> exchange >> noMoreImportantTokens) <|>
               try (fourDigitCountryCode >> areaCode >> localNumber >> noMoreImportantTokens) <|>
               try (fourDigitCountryCode >> areaCode >> fullLocalNumber >> extension >> noMoreImportantTokens) <|>

               try (countryCode >> bracketedAreaCode >> fullLocalNumber >> extension >> noMoreImportantTokens) <|>
               try (dashedCountryCode >> areaCode >> fullLocalNumber >> extension >> noMoreImportantTokens)



exchange :: Monad m => PhoneNumberParser m ()
exchange = do
  countRange 0 3 consumeExchangeDigit
  many (oneOf dash)
  return ()

localNumber :: Monad m => PhoneNumberParser m ()
localNumber = do
  count 3 consumeExchangeDigit
  many (oneOf dash)
  countRange 0 4 consumeSuffixDigit
  return ()

extension :: Monad m => PhoneNumberParser m ()
extension = do
  many (oneOf (extensionSeperator <> dash))
  many consumeExtensionDigit 
  return ()

qualifiedExtension :: Monad m => PhoneNumberParser m ()
qualifiedExtension = do
  many1 (oneOf (extensionSeperator <> dash))
  many consumeExtensionDigit 
  return ()

fullLocalNumber :: Monad m => PhoneNumberParser m ()
fullLocalNumber = do
  count 3 consumeExchangeDigit 
  many (oneOf dash)
  count 4 consumeSuffixDigit 
  return ()

openingAreaCode :: Monad m => PhoneNumberParser m ()
openingAreaCode = do
  oneOf lbrace
  countRange 0 3 consumeAreaCodeDigit 
  return ()

areaCodeBetween :: Monad m => ParsecT Text PhoneNumber m Char -> ParsecT Text PhoneNumber m Char  -> PhoneNumberParser m ()
areaCodeBetween l r = between l r areaCodeOnly >> return ()


areaCodeOnly :: Monad m => PhoneNumberParser m ()
areaCodeOnly = count 3 consumeAreaCodeDigit >> return ()

bracketedAreaCode :: Monad m => PhoneNumberParser m ()
bracketedAreaCode = areaCodeBetween (oneOf lbrace) (oneOf rbrace) >> many (oneOf dash) >> return ()

areaCode :: Monad m => PhoneNumberParser m ()
--dont care if braces are the same "flavor", as long as we get a left and a right one
areaCode = try (bracketedAreaCode) <|>
           try (areaCodeOnly >> many (oneOf dash) >> return ())


dashedCountryCode :: Monad m => PhoneNumberParser m ()
dashedCountryCode = try (count 4 consumeCountryCodeDigit >> many1 (oneOf dash) >> return ()) <|>
                    try (count 3 consumeCountryCodeDigit >> many1 (oneOf dash) >> return ()) <|>
                    try (count 2 consumeCountryCodeDigit >> many1 (oneOf dash) >> return ()) <|>
                    try (count 1 consumeCountryCodeDigit >> many1 (oneOf dash) >> return ()) 


oneDigitCountryCode :: Monad m => PhoneNumberParser m ()
oneDigitCountryCode = count 1 consumeCountryCodeDigit >> many (oneOf dash) >> return ()

twoDigitCountryCode :: Monad m => PhoneNumberParser m ()
twoDigitCountryCode = count 2 consumeCountryCodeDigit >> many (oneOf dash) >> return ()

threeDigitCountryCode :: Monad m => PhoneNumberParser m ()
threeDigitCountryCode = count 3 consumeCountryCodeDigit >> many (oneOf dash) >> return ()

fourDigitCountryCode :: Monad m => PhoneNumberParser m ()
fourDigitCountryCode = count 4 consumeCountryCodeDigit >> many (oneOf dash) >> return ()

countryCode :: Monad m => PhoneNumberParser m ()
countryCode = countRange 0 4 consumeCountryCodeDigit >> many (oneOf dash) >> return ()

noMoreImportantTokens :: Monad m => PhoneNumberParser m ()
noMoreImportantTokens = manyTill (noneOf (digit <> rbrace)) eof >> return ()

consumeDigit :: Monad m => (PhoneNumber -> Text -> PhoneNumber) -> PhoneNumberParser m ()
consumeDigit f = oneOf digit >>= \a -> do
  ph <- getState
  putState (f ph (singleton a))

consumeExchangeDigit :: Monad m => PhoneNumberParser m ()
consumeExchangeDigit = consumeDigit $ \ph a -> ph {_exchange = (_exchange ph) <> a}

consumeSuffixDigit :: Monad m => PhoneNumberParser m ()
consumeSuffixDigit = consumeDigit $ \ph a -> ph {_suffix = (_suffix ph) <> a}

consumeExtensionDigit :: Monad m => PhoneNumberParser m ()
consumeExtensionDigit = consumeDigit $ \ph a -> ph {_extension = (_extension ph) <> a}

consumeAreaCodeDigit :: Monad m => PhoneNumberParser m ()
consumeAreaCodeDigit = consumeDigit $ \ph a -> ph {_areaCode = (_areaCode ph) <> a}

consumeCountryCodeDigit :: Monad m => PhoneNumberParser m ()
consumeCountryCodeDigit = consumeDigit $ \ph a -> ph {_countryCode = (_countryCode ph) <> a}

countRange :: (Stream s m t, Monad m) => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
countRange min max parser = choice $ map (\x -> try(count x parser)) (reverse [min..max])
