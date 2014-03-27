``` {.sourceCode .literate .haskell}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
```

validations
===========

What is "validations"?
----------------------

``` {.sourceCode .literate .haskell}
 module Validations.Tutorial
--  ( posted
--  , contextValidate
--  , userValidation
--  ) 
     where
```

``` {.sourceCode .literate .haskell}
import Prelude hiding ((.))
import Validations.Internal.Lens(Lens, lens)
import Validations.Adapters.Digestive(validateView,testEnv)
import Text.Digestive.Form(Form, text, (.:))
import qualified Validations.Checkers.PhoneNumber as VPH
import Validations.Types.PhoneNumber as TPH
import Data.Text(Text)
import Control.Applicative((<$>), (<*>))
import Text.Digestive.View(View, postForm)
import Data.Monoid(Monoid(..), mempty)
import Control.Arrow((>>>))
import Validations.Validator(attach)
import Validations.Validation(Validation, validation)
```

``` {.sourceCode .literate .haskell}
data User = User
  { _firstName   :: Text
  , _lastName    :: Text
  , _email       :: Text
  , _phoneNumber :: TPH.PhoneNumber
  } deriving Show
```

``` {.sourceCode .literate .haskell}
instance Monoid User where
  mempty = User { _firstName = "", _lastName = "", _email = "", _phoneNumber = mempty}
  mappend = undefined
```

``` {.sourceCode .literate .haskell}
firstName :: Lens Text User
firstName = lens _firstName (\s a -> s {_firstName = a})
```

``` {.sourceCode .literate .haskell}
lastName :: Lens Text User
lastName  = lens _lastName (\s a -> s {_lastName = a})
```

``` {.sourceCode .literate .haskell}
email :: Lens Text User
email     = lens _email (\s a -> s {_email = a})
```

``` {.sourceCode .literate .haskell}
phoneNumber2 :: Lens PhoneNumber User
phoneNumber2 = lens _phoneNumber (\s a -> s {_phoneNumber = a})
```

``` {.sourceCode .literate .haskell}
notEmpty :: (Monoid a, Eq a) => a -> Either Text a
notEmpty x =
  if (x == mempty)
     then Left "is empty"
     else Right x
```

``` {.sourceCode .literate .haskell}
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f e = case e of
  Left x -> Left (f x)
  Right x -> Right x 
```

``` {.sourceCode .literate .haskell}
firstNameField :: Text
firstNameField     = "firstName"
```

``` {.sourceCode .literate .haskell}
lastNameField :: Text
lastNameField      = "lastName"
```

``` {.sourceCode .literate .haskell}
emailField :: Text
emailField         = "email"
```

``` {.sourceCode .literate .haskell}
emailConfirmField :: Text
emailConfirmField  = "emailConfirm"
```

``` {.sourceCode .literate .haskell}
phoneNumberField :: Text
phoneNumberField   = "phoneNumber"
```

``` {.sourceCode .literate .haskell}
userForm :: (Monad m) => Form Text m (Text,Text,Text,Text,Text)
userForm = (,,,,)
  <$> firstNameField    .: (text Nothing)
  <*> lastNameField     .: (text Nothing)
  <*> emailField        .: (text Nothing)
  <*> emailConfirmField .: (text Nothing)
  <*> phoneNumberField  .: (text Nothing)
```

``` {.sourceCode .literate .haskell}
--userValidation :: (Text, Text, Text, Text, Text) -> Validation [(Text, Text)] IO User User
 userValidation (f1, f2, f3,f4, f5) = 
   validation firstName f1 (
     notEmpty `attach` firstNameField
   )
   >>>
   validation lastName f2 (
     notEmpty `attach` lastNameField
   )
   >>>
   validation email f3 (
     notEmpty        `attach` emailField
     >>>
     (f4 `confirms`) `attach` emailConfirmField
   )
   >>>
   validation lastName  f5 (
     notEmpty `attach` emailConfirmField
   )
   >>>
   validation phoneNumber2 f5 (
     notEmpty                                                 `attach` phoneNumberField
     >>>
     (VPH.phoneNumber  >>> mapLeft (const "bad number"))      `attach` phoneNumberField
     >>> 
     (VPH.hasExtension >>> mapLeft (const "needs extension")) `attach` phoneNumberField
 
   ) 
```

``` {.sourceCode .literate .haskell}
confirms :: (Eq a) => a -> a -> Either Text a
confirms a b = case (a == b) of
  True -> Right b
  False -> Left "fields do not match."
```

``` {.sourceCode .literate .haskell}
{-
z :: (Monad m, Monoid v, Monoid t) => (s -> Validation Text v t m a b) -> m (View v, Maybe s) -> m (View v, Maybe t)
z = validateView
-}

posted :: (Monad m) => m (View Text, Maybe (Text,Text,Text,Text,Text))
posted = postForm "f" userForm $ testEnv [("f.firstName", "hello"), ("f.lastName", "world"), ("f.email", "hello@world.com"), ("f.emailConfirm", "hello@world.com"), ("f.phoneNumber", "1(333)333-3333x3")]

```
