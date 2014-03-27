validations
===========

What is "validations"?
----------------------

validations is a Haskell library that attempts to solve two problems.
First, it provides a flexible, composable way to define validations of a
domain model. It also includes a bunch of useful "checkers" that aren't
specific to any one domain model (e.g. a phone number checker, an email
checker, etc.) with localized error messages.

Existing solutions, and their problems
--------------------------------------

[jump to the "hello world" code example](#hello-world)

There is a number of ways to do domain model validation in Haskell, but
each current method has drawbacks. Let's imagine a simple user model:

``` {.sourceCode .literate .haskell}
data User = User
  { _firstName   :: Text
  , _lastName    :: Text
  , _email       :: Text
  , _phoneNumber :: TPH.PhoneNumber
  } deriving Show
```

We want to check that the first name is not empty and starts with the
letter A, the last lame is not empty, the email address is not empty,
and it is confirmed by a value that isn't stored in User. We also want
all checkers to conform to the type

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

``` {.sourceCode .literate .haskell}
startsWith :: Text -> Text -> Either Text Text
startsWith predicate input = 
  if predicate `isPrefixOf` input
     then Right input
     else Left $ "does not start with " <> predicate
```

``` {.sourceCode .literate .haskell}
confirms :: (Eq a) => a -> a -> Either Text a
confirms a b = case (a == b) of
  True -> Right b
  False -> Left "fields do not match."
```

### Smart Constructors ###

The simplest way to do this is with a smart constructor:

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
{-
z :: (Monad m, Monoid v, Monoid t) => (s -> Validation Text v t m a b) -> m (View v, Maybe s) -> m (View v, Maybe t)
z = validateView
-}

posted :: (Monad m) => m (View Text, Maybe (Text,Text,Text,Text,Text))
posted = postForm "f" userForm $ testEnv [("f.firstName", "hello"), ("f.lastName", "world"), ("f.email", "hello@world.com"), ("f.emailConfirm", "hello@world.com"), ("f.phoneNumber", "1(333)333-3333x3")]

```
