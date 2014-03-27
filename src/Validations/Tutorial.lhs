> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RankNTypes        #-}

> module Validations.Tutorial  where


> import Prelude hiding ((.))
> import Validations.Internal.Lens(Lens, lens)
> import Validations.Adapters.Digestive(validateView, testEnv)
> import Text.Digestive.Form(Form, text, (.:))
> import qualified Validations.Checkers.PhoneNumber as VPH
> import Validations.Types.PhoneNumber as TPH
> import Data.Text(Text, isPrefixOf)
> import Control.Applicative((<$>), (<*>))
> import Text.Digestive.View(View, postForm)
> import Data.Monoid(Monoid(..), mempty, (<>))
> import Control.Arrow((>>>))
> import Validations.Validator(attach)
> import Validations.Validation(Validation, validation)

validations
===========

What is "validations"?
----------------------

validations is a Haskell library that attempts to solve two problems.
First, it provides a flexible, composable way to define validations
of a domain model.  It also includes a bunch of useful "checkers"
that aren't specific to any one domain model (e.g. a phone number checker,
an email checker, etc.) with localized error messages.

Existing solutions, and their problems
--------------------------------------

[jump to the "hello world" code example](#hello-world)


There is a number of ways to do domain model validation in Haskell, but
each current method has drawbacks. Let's imagine a simple user model:

> data User = User
>   { _firstName   :: Text
>   , _lastName    :: Text
>   , _email       :: Text
>   , _phoneNumber :: TPH.PhoneNumber
>   } deriving Show

We want to check that the first name is not empty and starts with the letter
A, the last lame is not empty, the email address is not empty, and it is confirmed
by a value that isn't stored in User. We also want all checkers to conform to the
type 

< a -> Either e b

or 

< (Monad m) => a -> m (Either a b)

.

So, our checkers could look something like

> notEmpty :: (Monoid a, Eq a) => a -> Either Text a
> notEmpty x =
>   if (x == mempty)
>      then Left "is empty"
>      else Right x


> startsWith :: Text -> Text -> Either Text Text
> startsWith predicate input = 
>   if predicate `isPrefixOf` input
>      then Right input
>      else Left $ "does not start with " <> predicate

> confirms :: (Eq a) => a -> a -> Either Text a
> confirms a b = case (a == b) of
>   True -> Right b
>   False -> Left "fields do not match."

 ### Smart Constructors ###

The simplest way to do this is with a smart constructor:


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


> {-
> z :: (Monad m, Monoid v, Monoid t) => (s -> Validation Text v t m a b) -> m (View v, Maybe s) -> m (View v, Maybe t)
> z = validateView
> -}
> 
> posted :: (Monad m) => m (View Text, Maybe (Text,Text,Text,Text,Text))
> posted = postForm "f" userForm $ testEnv [("f.firstName", "hello"), ("f.lastName", "world"), ("f.email", "hello@world.com"), ("f.emailConfirm", "hello@world.com"), ("f.phoneNumber", "1(333)333-3333x3")]
> 
>
