validations
===========

What is "validations"?
----------------------

**validations** is a Haskell library that attempts to solve two
problems. First, it provides a flexible, composable way to define
validations of a domain model. It also includes a bunch of useful
"checkers" that aren't specific to any one domain model (e.g. a phone
number checker, an email checker, etc.) with localized error messages.

Existing solutions, and their problems
--------------------------------------

[jump to the "hello world" code example](#hello-world)

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

### Smart Constructors ###

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

Ideas behind the validation library structure
---------------------------------------------

[jump to the "hello world" code example](#hello-world)

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
categories (e.g. (\>=\>). However, we do now how to unwrap and rewrap
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

The final import data type is a *Validation*. The type of a validations
is:

    state -> monad (newState, errors)

where state is a type like a user record. newState is typically the same
type as state, but a transformation is allowed. Validations can be
constructed with

    validation :: (Monad m) => Lens b s -> a -> Validator ek ev m a b -> Validation [(ek,ev)] m s s

Validations also form a category and can be composed, similar to
Validators.
