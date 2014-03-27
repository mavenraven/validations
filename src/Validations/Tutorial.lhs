> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RankNTypes        #-}

validations
===========

What is "validations"?
----------------------

> module Validations.Tutorial
>--  ( posted
>--  , contextValidate
>--  , userValidation
>--  ) 
>     where


> import Prelude hiding ((.))
> import Validations.Internal.Lens(Lens, lens)
> import Validations.Adapters.Digestive(validateView,testEnv)
> import Text.Digestive.Form(Form, text, (.:))
> import qualified Validations.Checkers.PhoneNumber as VPH
> import Validations.Types.PhoneNumber as TPH
> import Data.Text(Text)
> import Control.Applicative((<$>), (<*>))
> import Text.Digestive.View(View, postForm)
> import Data.Monoid(Monoid(..), mempty)
> import Control.Arrow((>>>))
> import Validations.Validator(attach)
> import Validations.Validation(Validation, validation)

> data User = User
>   { _firstName   :: Text
>   , _lastName    :: Text
>   , _email       :: Text
>   , _phoneNumber :: TPH.PhoneNumber
>   } deriving Show


> instance Monoid User where
>   mempty = User { _firstName = "", _lastName = "", _email = "", _phoneNumber = mempty}
>   mappend = undefined

> firstName :: Lens Text User
> firstName = lens _firstName (\s a -> s {_firstName = a})

> lastName :: Lens Text User
> lastName  = lens _lastName (\s a -> s {_lastName = a})

> email :: Lens Text User
> email     = lens _email (\s a -> s {_email = a})

> phoneNumber2 :: Lens PhoneNumber User
> phoneNumber2 = lens _phoneNumber (\s a -> s {_phoneNumber = a})

> notEmpty :: (Monoid a, Eq a) => a -> Either Text a
> notEmpty x =
>   if (x == mempty)
>      then Left "is empty"
>      else Right x

> mapLeft :: (a -> c) -> Either a b -> Either c b
> mapLeft f e = case e of
>   Left x -> Left (f x)
>   Right x -> Right x 

> firstNameField :: Text
> firstNameField     = "firstName"

> lastNameField :: Text
> lastNameField      = "lastName"

> emailField :: Text
> emailField         = "email"

> emailConfirmField :: Text
> emailConfirmField  = "emailConfirm"

> phoneNumberField :: Text
> phoneNumberField   = "phoneNumber"

> userForm :: (Monad m) => Form Text m (Text,Text,Text,Text,Text)
> userForm = (,,,,)
>   <$> firstNameField    .: (text Nothing)
>   <*> lastNameField     .: (text Nothing)
>   <*> emailField        .: (text Nothing)
>   <*> emailConfirmField .: (text Nothing)
>   <*> phoneNumberField  .: (text Nothing)

>--userValidation :: (Text, Text, Text, Text, Text) -> Validation [(Text, Text)] IO User User
> userValidation (f1, f2, f3,f4, f5) = 
>   validation firstName f1 (
>     notEmpty `attach` firstNameField
>   )
>   >>>
>   validation lastName f2 (
>     notEmpty `attach` lastNameField
>   )
>   >>>
>   validation email f3 (
>     notEmpty        `attach` emailField
>     >>>
>     (f4 `confirms`) `attach` emailConfirmField
>   )
>   >>>
>   validation lastName  f5 (
>     notEmpty `attach` emailConfirmField
>   )
>   >>>
>   validation phoneNumber2 f5 (
>     notEmpty                                                 `attach` phoneNumberField
>     >>>
>     (VPH.phoneNumber  >>> mapLeft (const "bad number"))      `attach` phoneNumberField
>     >>> 
>     (VPH.hasExtension >>> mapLeft (const "needs extension")) `attach` phoneNumberField
> 
>   ) 

> confirms :: (Eq a) => a -> a -> Either Text a
> confirms a b = case (a == b) of
>   True -> Right b
>   False -> Left "fields do not match."

> {-
> z :: (Monad m, Monoid v, Monoid t) => (s -> Validation Text v t m a b) -> m (View v, Maybe s) -> m (View v, Maybe t)
> z = validateView
> -}
> 
> posted :: (Monad m) => m (View Text, Maybe (Text,Text,Text,Text,Text))
> posted = postForm "f" userForm $ testEnv [("f.firstName", "hello"), ("f.lastName", "world"), ("f.email", "hello@world.com"), ("f.emailConfirm", "hello@world.com"), ("f.phoneNumber", "1(333)333-3333x3")]
> 
>
