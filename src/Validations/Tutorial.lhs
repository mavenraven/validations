> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RankNTypes        #-}

> module Validations.Tutorial  where

> import Prelude hiding ((.))
> import Validations.Internal.Lens(Lens, lens)
> import Validations.Adapters.Digestive(validateView, testEnv)
> import Text.Digestive.Form(Form, text, (.:), validate, validateM)
> import Text.Digestive.Types(Result(..))
> import qualified Validations.Checkers.PhoneNumber as VPH
> import Validations.Types.PhoneNumber (PhoneNumber)
> import Data.Text(Text, isPrefixOf)
> import Control.Applicative((<$>), (<*>), (*>))
> import Text.Digestive.View(View, postForm)
> import Data.Monoid(Monoid(..), mempty, (<>))
> import Control.Arrow((>>>))
> import Control.Monad((>=>))
> import Validations.Validator(attach)
> import Validations.Validation(Validation, validation)
> import Control.Monad.State.Class(MonadState)


> eitherToResult x = case x of
>   Left x'  -> Error x'
>   Right x' -> Success x'


validations
===========

What is "validations"?
----------------------

**validations** is a Haskell library that attempts to solve two problems.
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
>   { _firstName    :: Text
>   , _lastName     :: Text
>   , _emailAddress :: Text
>   } deriving Show

We want to check that the first name is not empty and starts with the letter
A, that the last name is not empty, that the email address is not empty, and that 
the email address is confirmed by a value that isn't stored in User. 
We also want all checkers to conform to the type 

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

,

> startsWith :: Text -> Text -> Either Text Text
> startsWith predicate input = 
>   if predicate `isPrefixOf` input
>      then Right input
>      else Left $ "does not start with " <> predicate

, and

> confirms :: (Eq a) => a -> a -> Either Text a
> confirms a b = case (a == b) of
>   True -> Right b
>   False -> Left "fields do not match."

.

 ### Smart Constructors ###

The simplest way to do this is with a smart constructor:

> user :: Text -> Text -> Text -> Text -> Either Text User
> user firstName lastName emailAddress emailAddressConfirm = do
>   firstName'    <- notEmpty firstName >>= startsWith "A"
>   lastName'     <- notEmpty lastName
>   emailAddress' <- notEmpty emailAddress
>   confirmed     <- emailAddressConfirm `confirms` emailAddress
>   return $ User {_firstName = firstName', _lastName = lastName', _emailAddress = confirmed }

This will enforce all of our invariants, but there's a problem. If any of our validations
fail, then we only get the results of the failure of first validation. If firstName and
lastName are both empty, we'd like to know that the validation logic failed for both. Also, if we
use the pattern of exposing only the smart constructor (user), and keeping the data constructor (User)
hidden, then a User record can only be used in contexts where all the invariants must always be held,
which can be inflexible.


 ### digestive-functors formlet style ###

**digestive-functors** solves the multiple validations problem. Our formlet could look like:

> userForm :: (Monad m) => Form Text m User
> userForm = User
>   <$>  "firstName"    .: validate ((notEmpty >=> startsWith "A") >>> eitherToResult) (text Nothing)
>   <*>  "lastName "    .: validate  (notEmpty >>> eitherToResult)                     (text Nothing)
>   <*>  "emailAddress" .: validate  (notEmpty >>> eitherToResult)                     (text Nothing)

But, how do we handle the email confirmations? Since formlets are applicatives
and not monadic, one way we could do this is by threading state through the monad and using validateM,
but that's mistake prone, and puts unnecessary constraints on our monad. We could intercept the View
record from **digestive-functors** once the form has been rendered, but then we're splitting our validation
logic into two different places.


Ideas behind the validation library structure
---------------------------------------------

[jump to the "hello world" code example](#hello-world)

**validations** is based around 4 different data types. First, a *Checker* is
function with type

< a -> Either e b

Checkers tend to be non domain model specific, reusable pieces of code. For
example, 

> nonEmpty :: (Monoid a, Eq a) => a -> Either Text a
> nonEmpty x =
>   if (x == mempty)
>      then Left "is empty"
>      else Right x

Notice that a Checker can transform its input as well, which consumer is free
to ignore. This is useful for turning string typed user input into structured data.


A *Monadic Checker* is the same as a checker, but with type

< (Monad m) => a -> m (Either e b)

.

The next data type is a *Validator*. It's a function with type

< a -> monad (Either (errorKey, errorValue) b)

It's very similar to a Monadic Checker, but it also uses an "errorKey" type.
This allows us to map a validator failure back to some given input (e.g. a form input field).
If you look at the type signature for a Validator, you'll notice that it's very similar to

< a -> m b


> {-
> instance Monoid User where
>   mempty = User { _firstName = "", _lastName = "", _emailAddress = "", _phoneNumber = mempty}
>   mappend = undefined

> firstName :: Lens Text User
> firstName = lens _firstName (\s a -> s {_firstName = a})

> lastName :: Lens Text User
> lastName  = lens _lastName (\s a -> s {_lastName = a})

> emailAddress :: Lens Text User
> emailAddress = lens _emailAddress (\s a -> s {_emailAddress = a})

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

> {-
> phoneNumberField :: Text
> phoneNumberField   = "phoneNumber"
> -}

> {-
> userForm :: (Monad m) => Form Text m (Text,Text,Text,Text,Text)
> userForm = (,,,,)
>   <$> firstNameField    .: (text Nothing)
>   <*> lastNameField     .: (text Nothing)
>   <*> emailField        .: (text Nothing)
>   <*> emailConfirmField .: (text Nothing)
>   <*> phoneNumberField  .: (text Nothing)
> -}

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
>   validation emailAddress f3 (
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
> -}
