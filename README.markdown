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

There is a number of ways to do domain model validation in Haskell, but
each current method has drawbacks. Let's imagine a simple user model:

    data User = User
      { _firstName    :: Text
      , _lastName     :: Text
      , _emailAddress :: Text
      } deriving Show

We want to check that the first name is not empty and starts with the letter
A, the last lame is not empty, the email address is not empty, and it is confirmed
by a value that isn't stored in User.


### Smart Constructors ###

The simplest way to do this is with a smart constructor:

    -- assume:
    -- notEmpty   :: Text -> Either Text Text
    -- startsWith :: Text -> Text -> Either Text Text
    -- confirm    :: Text -> Text -> Either Text Text

    user :: Text -> Text -> Text -> Text -> Either User
    user firstName lastName emailAddress emailAddressConfirm = do
      firstName'    <- notEmpty firstName >>= startsWith "A"
      lastName'     <- notEmpty lastName
      emailAddress' <- notEmpty emailAddress
      confirmed     <- confirm emailAddress emailAddressConfirm
      return $ User {_firstName = firstName', _lastName = lastName', _emailAddress = confirmed }
      

This will enforce all of our invariants, but there's a problem. If any of our validations
fail, then we only get the results of the failure of first validation. If firstName and
lastName are both empty, we'd like to know that the validation logic failed for both. Also, if we
use the pattern of exposing only the smart constructor (user, and keeping the data Constructor (User)
hidden, then a User record can only be used in contexts where all the invariants must always be held,
which can be inflexible.


### digestive-functors formlet style ###

digestive-functors solves the multiple validations problem. Our formlet could look like:

    -- assume:
    -- notEmpty   :: Text -> Result Text Text
    -- startsWith :: Text -> Text -> Result Text Text
    -- confirm    :: Text -> Text -> Result Text Text

    userForm :: (Monad m) => Form Text m User
    userForm = User <$>
    <*>  "firstName" .: validate (notEmpty >=> startsWith "A") (text Nothing)
    <*>  "lastName " .: validate notEmpty (text Nothing)
    <*>  "email"     .: validate notEmpty (text Nothing)

But, how do we handle the email confirmations? Since formlets are applicatives
and not monadic, it looks like the only we could do it is something like:

    userForm :: (MonadState s m) => Form Text m User
    userForm = User <$>
    <*>  "firstName"    .: validate (notEmpty >=> startsWith "A") (text Nothing)
    <*>  "lastName "    .: validate notEmpty (text Nothing)
    <*>  "email"        .: validateM (return . notEmpty >=> willConfirm) (text Nothing)
    <*>  "emailConfirm" .: validateM confirms (text Nothing)

Unfortunately, this has its own set of problems. The biggest, at least with how digestive-functors
is currently structured, is that



security numbers, and more. It also provides localized error messages for 
invalid inputs, which can be used in conjuction with the accept headers in
Snap to provide out of the box internationalization.  It's based on the idea of
[Active Record validations](http://edgeguides.rubyonrails.org/active_record_validations.html)
, but without all the callbacks. Though this library is aimed at working
digestive-functors, it should be reusable in other contexts as well.


Currently Supported Parsers and Validators:
-------------------------------------------

phone numbers


Planned Supported Parsers/ Domain Types
-------------------------------------------

ip v4 / v6 addresses

gender

sex

ethnicity

race

disability

social security numbers

education

profession

credit card number

bank account number

routing number

marital status

email address

http address

zip code

street address \[maybe not\]

dates/times (in separate haskell-chronic package)

currency

common quantities (e.g. "5 meters")

vin  / license \[probably not\]

uuid

coordinates


Planned supported Validators
----------------------------

confirmations (e.g. email confirmation)

localIpAddress

various email validators

currency in range, positive, etc.

must accept (e.g. TOS)

lengths

uniqueness 

unicode name (i.e. 専門家 is fine, but    😸 is not)

geohashing and distance (how exotic!)



Help
----

To build:

git clone https://github.com/mavenraven/digestive-functors-validations

cd digestive-functors-validations

cabal sandbox init

cabal install --enable-test

Only phone numbers are working so far, so there is plenty to be worked on.
Or, if you don't want to write code but know a foreign language, it would
be great to add what you know to any needed localized strings. Or, if you
can think of a useful to parse data type, file an issue! Thanks for any help!


Related Projects
----------------

[Snap](https://github.com/snapframework)

[digestive-functors](https://github.com/jaspervdj/digestive-functors)

Credits
-------

Authors:

- [mavenraven.org](http://www.mavenraven.org/)
