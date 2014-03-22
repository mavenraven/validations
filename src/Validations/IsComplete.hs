{-# LANGUAGE OverloadedStrings #-}

module Validations.IsComplete
  ( IsComplete(..)
  ) where

import Prelude hiding (length)
import Validations.Localization(LocalizedMessage)
import Validations.Types.PhoneNumber(PhoneNumber(..))
import Validations.Localization(english, Message(..), message)
import Data.Text(length)


-- | All parsers should be liberal in what they accept, and return
-- sucessfully even for framgments. For example, \"3\" for a phone number
-- should yield
-- 
-- > PhoneNumber 
-- >  { _countryCode = ""
-- >  , _areaCode    = ""
-- >  , _exchange    = "3"
-- >  , _suffix      = ""
-- >  , _extension   = ""
-- >  }
--
-- . This allows us responsively (think ajax) format the user's input and
-- give them feedback on what they have entered. However,
-- we want a way to differentiate completed domain objects and ones
-- that are fragments in a form submission. So, all domain objects
-- should be instances of 'IsComplete'.
class IsComplete a where
  isComplete ::  a -> Either LocalizedMessage a

instance IsComplete PhoneNumber where
  isComplete x =
    if (length (_exchange x) >= 3) 
       then Right x
       else Left $ message
         [ Message english Nothing "Phone number is not valid." 
         ]
