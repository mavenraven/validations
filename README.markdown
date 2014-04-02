validations
===========

table of contents
-----------------

[what is "validations"?](#what-is-validations)

[existing solutions, and their
problems](#existing-solutions-and-their-problems)

    [smart constructors](#smart-constructors)

    [digestive-functors formlet
style](#digestive-functors-formlet-style)

[ideas behind the structure of
"validations"](#ideas-behind-the-structure-of-validations)

[hello world](#hello-world)

[testing](#testing)

[digestive-functors integration](#digestive-functors-integration)

What is "validations"?
----------------------

**validations** is a Haskell library that attempts to solve two
problems. First, it provides a flexible, composable way to define
validations of a domain model. It also includes a bunch of useful
"checkers" that aren't specific to any one domain model (e.g. a phone
number checker, an email checker, etc.) with localized error messages.

existing solutions, and their problems
--------------------------------------

There is a number of ways to do domain model validation in Haskell, but
each current method has drawbacks. Let's imagine a simple user model:

``` {.sourceCode .literate .haskell}
data User = User
  { _firstName    :: Text
  , _lastName     :: Text
  , _emailAddress :: Text
  } deriving Show
```

We want to check that the first name is not empty and starts with the
letter A, that the last name is not empty, that the email address is not
empty, and that the email address is confirmed by a value that isn't
stored in User. We also want all checkers to conform to the type

``` {.sourceCode .haskell}
a -> Either e b
```

or

``` {.sourceCode .haskell}
(Monad m) => a -> m (Either a b)
```

.

So, our checkers could look something like

``` {.sourceCode .literate .haskell}
notEmpty :: (Monoid a, Eq a) => a -> Either Text a
notEmpty x =
  if (x == mempty)
     then Left "is empty"
     else Right x
```

,

``` {.sourceCode .literate .haskell}
startsWith :: Text -> Text -> Either Text Text
startsWith predicate input = 
  if predicate `isPrefixOf` input
     then Right input
     else Left $ "does not start with " <> predicate
```

, and

``` {.sourceCode .literate .haskell}
confirms :: (Eq a) => a -> a -> Either Text a
confirms a b = case (a == b) of
  True -> Right b
  False -> Left "fields do not match."
```

.

### smart constructors ###

The simplest way to do this is with a smart constructor:

``` {.sourceCode .literate .haskell}
user :: Text -> Text -> Text -> Text -> Either Text User
user firstName lastName emailAddress emailAddressConfirm = do
  firstName'    <- notEmpty firstName >>= startsWith "A"
  lastName'     <- notEmpty lastName
  emailAddress' <- notEmpty emailAddress
  confirmed     <- emailAddressConfirm `confirms` emailAddress
  return $ User {_firstName = firstName', _lastName = lastName', _emailAddress = confirmed }
```

This will enforce all of our invariants, but there's a problem. If any
of our validations fail, then we only get the results of the failure of
first validation. If firstName and lastName are both empty, we'd like to
know that the validation logic failed for both. Also, if we use the
pattern of exposing only the smart constructor (user), and keeping the
data constructor (User) hidden, then a User record can only be used in
contexts where all the invariants must always be held, which can be
inflexible.

### digestive-functors formlet style ###

**digestive-functors** solves the multiple validations problem. Our
formlet could look like:

``` {.sourceCode .literate .haskell}
userForm :: (Monad m) => Form Text m User
userForm = User
  <$>  "firstName"    .: validate ((notEmpty >=> startsWith "A") >>> eitherToResult) (text Nothing)
  <*>  "lastName "    .: validate  (notEmpty >>> eitherToResult)                     (text Nothing)
  <*>  "emailAddress" .: validate  (notEmpty >>> eitherToResult)                     (text Nothing)
```

But, how do we handle the email confirmations? Since formlets are
applicatives and not monadic, one way we could do this is by threading
state through the monad and using validateM, but that's mistake prone,
and puts unnecessary constraints on our monad. We could intercept the
View record from **digestive-functors** once the form has been rendered,
but then we're splitting our validation logic into two different places.

ideas behind the structure of "validations"
-------------------------------------------

**validations** is based around 4 different data types. First, a
*Checker* is function with type

``` {.sourceCode .haskell}
a -> Either e b
```

Checkers tend to be non domain model specific, reusable pieces of code.
For example,

``` {.sourceCode .literate .haskell}
nonEmpty :: (Monoid a, Eq a) => a -> Either Text a
nonEmpty x =
  if (x == mempty)
     then Left "is empty"
     else Right x
```

. Notice that a Checker can transform its input as well, which consumer
is free to ignore. This is useful for turning string typed user input
into structured data.

A *Monadic Checker* is the same as a checker, but with type

``` {.sourceCode .haskell}
(Monad m) => a -> m (Either e b)
```

.

The next data type is a *Validator*. It's a function with type

``` {.sourceCode .haskell}
a -> monad (Either (errorKey, errorValue) b)
```

It's very similar to a Monadic Checker, but it also uses an "errorKey"
type. This allows us to map a validator failure back to some given input
(e.g. a form input field). If you look at the type signature for a
Validator, you'll notice that it's very similar to

``` {.sourceCode .haskell}
a -> m b
```

, so it's a [Kleisli
category](http://www.haskell.org/haskellwiki/Monad_laws), but the Either
inside doesn't allow us to use Haskell stuff for composing Kleisi
categories (e.g. (\>=\>)). However, we do now how to unwrap and rewrap
Either, so validations provides an instance of Category to allow for
validator composition. To create validators, we typically combine either
a checker with a field name using

``` {.sourceCode .haskell}
attach :: (Monad m) => Checker ev a b -> ek -> Validator ek ev m a b
```

or

``` {.sourceCode .haskell}
attachM :: (Monad m) => MonadicChecker ev m a b -> ek -> Validator ek ev m a b
```

for monadic checkers. Both attach and attachM are included with
validations, but you're free to wrap any conforming function as the
Validator data constructor is public.

The final important data type is a *Validation*. The type of a
validations is:

    state -> monad (newState, errors)

where state is a type like a user record. newState is typically the same
type as state, but a transformation is allowed. Validations can be
constructed with

    validation :: (Monad m) => Lens b s -> a -> Validator ek ev m a b -> Validation [(ek,ev)] m s s

Validations also form a category and can be composed, similar to
Validators.

hello world
-----------

Let's see the Validators and Validations in action. First, let's define
a Account record:

``` {.sourceCode .literate .haskell}
data Account = Account
  { _name        :: Text
  , _phoneNumber :: PhoneNumber
  } deriving Show
```

PhoneNumber is a record type included with **validations** that allows
access to a phone number's exchange, extension, etc. Next, we want to
define lenses for accessing and mutating the fields. In this example, we
are using the internal lens functionality of **validations**, but we'd
typically use something like
[lens](http://hackage.haskell.org/package/lens) in our application.

``` {.sourceCode .literate .haskell}
name :: Lens Text Account
name = lens _name (\s a -> s {_name = a})
```

``` {.sourceCode .literate .haskell}
phoneNumber :: Lens PhoneNumber Account
phoneNumber = lens _phoneNumber (\s a -> s {_phoneNumber = a})
```

We also want to use **digestive-functors** to define our form to bring
data in.

``` {.sourceCode .literate .haskell}
nameField        = "name"
confirmNameField = "confirmName"
phoneNumberField = "phoneNumber"
```

``` {.sourceCode .literate .haskell}
accountForm :: (Monad m) => Form Text m (Text, Text, Text)
accountForm = (,,)
  <$> nameField        .: (text Nothing)
  <*> confirmNameField .: (text Nothing)
  <*> phoneNumberField .: (text Nothing)
```

We don't use field names directly in our formlet because we need to use
them in our validation as well. Also, notice that we are outputting a
3-tuple instead of a Account record. This is because there isn't a one
to one correspondence between our input fields and Account record fields
(the name confirm field will be discarded). So, our validation looks
like

``` {.sourceCode .literate .haskell}
accountValidation :: (Monad m) => (Text, Text, Text) -> Validation [(Text, Text)] m Account Account
accountValidation (f1, f2, f3) = 
  validation name f1 (
    notEmpty        `attach` nameField
    >>>
    (f2 `confirms`) `attach` confirmNameField
  )
  >>>
  validation phoneNumber f3 (
    notEmpty                                                 `attach` phoneNumberField
    >>>
    (VPH.phoneNumber  >>> mapLeft (const "bad number"))      `attach` phoneNumberField
    >>> 
    (VPH.hasExtension >>> mapLeft (const "needs extension")) `attach` phoneNumberField
  ) 
```

What's going on here? First, since both Validations and Validators are
Categories, we can use the (\>\>\>) operator from Control.Arrow. If
you're unfamiliar with this operator, for normal functions,

``` {.sourceCode .haskell}
a >>> b ≡ b . a
```

so you can think of it as a composition operator that reads left to
right.

Next, let's take a look at our first validation. It takes the "f1"
parameter passed into "accountValidation" and feeds it into "notEmpty".
If "notEmpty" returns a "Left", then the validation will make no state
changes, and instead adds

``` {.sourceCode .haskell}
("name" , "is empty")
```

to its "errors" value. On the other hand, if "f1" is not empty, it will
be passed onto the confirms function, where similar validation will
happen. If confirms also succeeds, the outputted value will passed to
the "name" lens, and the outputted state will be mutated with a new name
value.

Similar behavior will happen in our other validation, but there are two
important things to note. First, "VPH.phoneNumber" does not simply
output "Right f3" on success, but transforms f3 intro a PhoneNumber
record as well. Also, we are using mapLeft in this case because the
phone number checkers that come with **validations** have localized
messages, but for simplicity we're just using "Text".

testing
-------

Testing a validation is simple. For example,

``` {.sourceCode .literate .haskell}
_ = runValidation  (accountValidation ("hi", "hi", "313-333-3334x33")) Account { _name = "", _phoneNumber = mempty } :: Identity (Account, [(Text, Text)])
```

yields

``` {.sourceCode .haskell}
Account {_name = "hi", _phoneNumber = PhoneNumber {_countryCode = "", _areaCode = "313", _exchange = "333", _suffix = "3334", _extension = "33"}},[])
```

.

``` {.sourceCode .literate .haskell}
_ = runValidation  (accountValidation ("hi", "hi", "313-333-3334x33")) Account { _name = "", _phoneNumber = mempty } :: Identity (Account, [(Text, Text)]) --_
```

yields

``` {.sourceCode .haskell}
(Account {_name = "hi", _phoneNumber = PhoneNumber {_countryCode = "", _areaCode = "313", _exchange = "333", _suffix = "3334", _extension = "33"}},[])
```

.

``` {.sourceCode .literate .haskell}
_ = runValidation  (accountValidation ("hi", "bye", "313-333-3334")) Account { _name = "", _phoneNumber = mempty } :: Identity (Account, [(Text, Text)])
```

yields

``` {.sourceCode .haskell}
(Account {_name = "", _phoneNumber = PhoneNumber {_countryCode = "", _areaCode = "", _exchange = "", _suffix = "", _extension = ""}},[("confirmName","fields do not match."),("phoneNumber","needs extension")])
```

.

digestive-functors integration
------------------------------

Integration with **digestive-functors** is also pretty simple. Instead
of

``` {.sourceCode .literate .haskell}
posted :: (Monad m) => m (View Text, Maybe (Text,Text,Text))
posted = postForm "f" accountForm $ testEnv [("f.name", "hello"), ("f.confirmName", "hello"), ("f.phoneNumber", "1(333)333-3333x3")]
```

add validateView in as well, like

``` {.sourceCode .literate .haskell}
validatedPosted :: (Monad m) => m (View Text, Maybe Account)
validatedPosted =  posted >>= validateView accountValidation Account { _name = "", _phoneNumber = mempty }
```

You can also use validateView' if your domain record has a Monoid
instance.
