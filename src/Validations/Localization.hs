{-# LANGUAGE OverloadedStrings #-}

module Validations.Localization
    ( LanguageCode(..)
    , Clause
    , Sentence
    , Message(..)
    , LocalizedMessage
    , english
    , dutch
    , french
    , message
    )
    where

import Data.Map (Map, fromList)
import Data.Text(Text)

data LanguageCode = EN
 | EN_AUS
 | EN_BZ
 | EN_CA
 | EN_GB
 | EN_IE
 | EN_JM
 | EN_NZ
 | EN_TT
 | EN_US
 | EN_ZA
 | NL
 | NL_BE
 | FR
 | FR_BE
 | FR_CA
 | FR_CH
 | FR_LU
 deriving (Eq, Show, Ord, Enum)



english :: [LanguageCode]
english = [EN .. EN_ZA]

dutch :: [LanguageCode]
dutch = [NL .. NL_BE]

french :: [LanguageCode]
french = [FR .. FR_LU]


type Clause = Text
type Sentence = Text
type LocalizedMessage = Map LanguageCode (Maybe Clause, Sentence)

data Message = Message
  { _languageCodes :: [LanguageCode]
  , _clause        :: Maybe Clause
  , _sentence      :: Sentence
  }

-- | If language codes overlap, it is specified that the last one will take precedence. For example:
--
-- > message
-- >   [Message english                "must be less than 100 meters" "Height must be less than 100 meters."
-- >   ,Message [EN_GB, EN_AUS, EN_NZ] "must be less than 100 metres" "Height must be less than 100 metres."
-- >   ]
-- 
-- =
--
-- > fromList [(EN_US, ("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_GB, ("must be less than 100 metres","Height must be less than 100 metres.")),
-- >           (EN_AUS,("must be less than 100 metres","Height must be less than 100 metres.")),
-- >           (EN_NZ, ("must be less than 100 metres","Height must be less than 100 metres.")),
-- >           (EN_ZA, ("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN,    ("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_CA, ("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_IE, ("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_JM, ("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_BZ, ("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_TT, ("must be less than 100 meters","Height must be less than 100 meters."))]
--
-- .
-- This allows us define a general translation for a language, and to override with more specific dialects
-- as needed.
message :: [Message] -> LocalizedMessage
message ms = fromList $ do 
  m <- ms
  lc <- _languageCodes m
  [(lc, (_clause m, _sentence m))]







