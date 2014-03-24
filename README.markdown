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

    data User = User
      { _firstName    :: Text
      , _lastName     :: Text
      , _emailAddress :: Text
      } deriving Show

We want to check that the first name is not empty and starts with the letter
A, the last lame is not empty, the email address is not empty, and it is confirmed
by a value that isn't stored in User. We also want all checkers to conform to the
type 

    a -> Either e b

or

    (Monad m) => a -> m (Either a b)
.

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
use the pattern of exposing only the smart constructor (user), and keeping the data constructor (User)
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
is currently structured, is that it's hard to escape the monad if we just wanted to use the state
for evaluating the form. Even without this limitation, we are still passing state around in awkward
way that doesn't really show our intent. Also, by mixing our validation logic with our formlet rendering
logic, we are making it harder to test and reuse our validation logic. So, while using digestive-functor
style is workable, validations tries to offer a cleaner way.

Ideas behind the validation library structure
---------------------------------------------

[jump to the "hello world" code example](#hello-world)


validations is based around 4 different data types. First, a *Checker* is
function with type

    a -> Either e b

Checkers tend to be non domain model specific, reusable pieces of code. For
example, 

    nonEmpty :: (Monoid a, Eq a) => a -> Either Text a
    nonEmpty x =
      if (x == mempty)
        then Left "cannot be empty"
        else Right x

Notice that a checker can transform its input as well, which a callee is free
to ignore.


A *Monadic Checker* is the same as a checker, but with type

    (Monad m) => a -> m (Either e b)
.


The next data type is a *Validator*. It's a function with type

      a -> monad (Either (errorKey, errorValue) b)

It's very similar to a monadic checker, but it also uses an "errorKey" type.
This allows us to map a validator failure back to some given input (e.g. a form input field).
If you look at the type signature for a validator, you'll notice that it's very similar to

    a -> m b

, so it's a [Kleisli category](http://www.haskell.org/haskellwiki/Monad_laws),
but the Either inside doesn't allow us to use Haskell stuff for composing Kleisi categories (e.g. (>=>).
However, we do now how to unwrap and rewrap Either, so validations provides an instance of 
Category to allow for validator composition. To create validators, we typically combine either a
checker with a field name using

    attach :: (Monad m) => Checker ev a b -> ek -> Validator ek ev m a b

or 

    attachM :: (Monad m) => MonadicChecker ev m a b -> ek -> Validator ek ev m a b

for monadic checkers. Both attach and attachM are included with validations, but you're free
to wrap any conforming function as the Validator data constructor is public.

The final import data type is a *Validation*. The type of a validations is:

    state -> monad (newState, errors)

where state is a type like a user record. newState is typically the same type as state, but
a transformation is allowed. Validations can be constructed with

    validation :: (Monad m) => Lens b s -> a -> Validator ek ev m a b -> Validation [(ek,ev)] m s s

Validations also form a category and can be composed, similar to validators.


hello world
-----------

(all code is included in src/Validations/Tutorial.hs)
Let's see the validators and validations in action. First, let's define a User record:
    data User = User
    { _firstName   :: Text
    , _lastName    :: Text
    , _email       :: Text
    , _phoneNumber :: PhoneNumber
    } deriving Show

PhoneNumber is a record type included with validations that allows access to a phone
number's exchange, extension, etc. Next, we want to define lenses for accessing and
mutating the fields. In this example, we are using validations internal lens functionality,
but we'd typically use something like [lens](http://hackage.haskell.org/package/lens) in our
application. 

    firstName :: Lens Text User
    firstName = lens _firstName (\s a -> s {_firstName = a})

    lastName :: Lens Text User
    lastName  = lens _lastName (\s a -> s {_lastName = a})

    email :: Lens Text User
    email     = lens _email (\s a -> s {_email = a})

    phoneNumber :: Lens PhoneNumber User
    phoneNumber = lens _phoneNumber (\s a -> s {_phoneNumber = a})


We also want to use digestive-functors to define our form to bring data in.

    firstNameField     = "firstName"
    lastNameField      = "lastName"
    emailField         = "email"
    emailConfirmField  = "emailConfirm"
    phoneNumberField   = "phoneNumber"

    userForm :: (Monad m) => Form Text m (Text,Text,Text,Text,Text)
    userForm = (,,,,)
    <$> firstNameField    .: (text Nothing)
    <*> lastNameField     .: (text Nothing)
    <*> emailField        .: (text Nothing)
    <*> emailConfirmField .: (text Nothing)
    <*> phoneNumberField  .: (text Nothing)

We don't use field names directly in our formlet because we need to use
them in our validation as well. Also, notice that we are outputting a 5-tuple
instead of a User record. This is because there isn't a one to one correspondence
between our input fields and User record fields (the email confirm field will be discarded).
So, our validation looks like

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
