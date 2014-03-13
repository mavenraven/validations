{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Validations.Localization
    ( LanguageCode(..)
    , Clause
    , Sentence
    , LocalizedMessage(..)
    , english
    , dutch
    , message
    )
    where

import Data.Map (Map, fromList, insert)
import Data.Text(Text)

data LanguageCode = EN_US
 | EN_GB
 | EN_AUS
 | EN_NZ
 | EN_ZA
 | EN
 | EN_CA
 | EN_IE
 | EN_JM
 | EN_BZ
 | EN_TT
 | NL_BE
 | NL
 | FR
 | FR_BE
 | FR_CA
 | FR_CH
 | FR_LU
 deriving (Eq, Show, Ord)



english =
  [ EN_US
  , EN_GB
  , EN_AUS
  , EN_NZ
  , EN_ZA
  , EN
  , EN_CA
  , EN_IE
  , EN_JM
  , EN_BZ
  , EN_TT
  ]

dutch = 
  [ NL_BE
  , NL
  ]

french = 
  [ FR
  , FR_BE
  , FR_CA
  , FR_CH
  , FR_LU
  ]


type Clause = Text
type Sentence = Text
data LocalizedMessage = LocalizedMessage
  { _languageCodes :: [LanguageCode]
  , _clause        :: Maybe Clause
  , _sentence      :: Sentence
  }

-- | If language codes overlap, it is specified that the last one will take precedence. For example:
--
-- > message
-- >   [LocalizedMessage english                "must be less than 100 meters" "Height must be less than 100 meters."
-- >   ,LocalizedMessage [EN_GB, EN_AUS, EN_NZ] "must be less than 100 metres" "Height must be less than 100 metres."
-- >   ]
-- 
-- =
--
-- > fromList [(EN_US,("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_GB,("must be less than 100 metres","Height must be less than 100 metres.")),
-- >           (EN_AUS,("must be less than 100 metres","Height must be less than 100 metres.")),
-- >           (EN_NZ,("must be less than 100 metres","Height must be less than 100 metres.")),
-- >           (EN_ZA,("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN,("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_CA,("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_IE,("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_JM,("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_BZ,("must be less than 100 meters","Height must be less than 100 meters.")),
-- >           (EN_TT,("must be less than 100 meters","Height must be less than 100 meters."))]
--
-- .
-- This allows us define a general translation for a language, and to override with more specific dialects
-- as needed.
message :: [LocalizedMessage] -> Map LanguageCode (Maybe Clause, Sentence)
message lms = fromList $ do 
  lm <- lms
  lc <- _languageCodes lm
  [(lc, (_clause lm, _sentence lm))]







