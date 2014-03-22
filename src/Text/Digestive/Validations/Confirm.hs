{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Text.Digestive.Validations.Confirm
--  ( posted
--  , contextValidate
--  , userValidation
--  ) 
    where

import Prelude hiding ((.))
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Class(lift)
import Text.Digestive.Validations.Internal.Lens(getter, setter, Lens, firstTuple, lens)
import Text.Digestive.Form(Form, validateM, text, (.:))
import qualified Text.Digestive.Validations.PhoneNumber as VPH
import Text.Digestive.Validations.Types.PhoneNumber as TPH
import Text.Digestive.Form
import Text.Digestive.Form.Encoding
import Text.Digestive.Types
import Control.Applicative
import Data.Text(Text)
import Debug.Trace
import Control.Monad
import Data.Monoid
import Text.Digestive.View
import Control.Arrow((>>>))
import Control.Category
import Control.Monad.Trans
import Data.List(nub)

data User = User
  { _firstName   :: Text
  , _lastName    :: Text
  , _email       :: Text
  , _phoneNumber :: TPH.PhoneNumber
  } deriving Show


instance Monoid User where
  mempty = User { _firstName = "", _lastName = "", _email = "", _phoneNumber = mempty}
  mappend = undefined

firstName :: Lens Text User
firstName = lens _firstName (\s a -> s {_firstName = a})
lastName :: Lens Text User
lastName  = lens _lastName (\s a -> s {_lastName = a})
email :: Lens Text User
email     = lens _email (\s a -> s {_email = a})
phoneNumber2 :: Lens PhoneNumber User
phoneNumber2 = lens _phoneNumber (\s a -> s {_phoneNumber = a})

notEmpty x =
  if (x == mempty)
     then Left "is empty"
     else Right x

type Checker error a b = a -> Either error b
type MonadicChecker error monad a b = a -> monad (Either error b)

attach :: (Monad m) => Checker ev a b -> ek -> Validator ek ev m a b
attach validator fieldName = Validator $ \x -> case (validator x) of
  Right x' -> return $ Right x'
  Left  e   -> return $ Left (fieldName, e)

attachM :: (Monad m) => MonadicChecker ev m a b -> ek -> Validator ek ev m a b
attachM validator fieldName = Validator $ \x -> validator x >>= \x -> case x of
  Right x' -> return $ Right x'
  Left  e   -> return $ Left (fieldName, e)

coolness x = do
  print "hi"
  return $ Right x



mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f e = case e of
  Left x -> Left (f x)
  Right x -> Right x 

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

userValidation :: (Text, Text, Text,Text,Text) -> Validation Text Text User IO Text PhoneNumber
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
    notEmpty `attach` emailField
    >>>
    (f4 `confirms`) `attach` emailConfirmField
  )
  >>>
  validation_  f5 (
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

newtype Validator errorKey errorValue monad a b  = Validator { getValidator :: a -> monad (Either (errorKey, errorValue) b) }

instance (Monad m) => Category (Validator ek ev m) where
  id = Validator (\x -> return (Right x))
  y . x = Validator (compose (getValidator x) (getValidator y))

compose :: (Monad m) => (a -> m (Either (k, v) b)) -> (b -> m (Either (k, v) c)) -> (a -> m (Either (k, v) c))
compose f g = (\a -> f a >>= \rb -> case rb of
  Right b     -> g b
  Left   (fn,e) -> return (Left (fn, e)))

runValidation' :: (Monad m, Monoid s) => Validation ek ev s m a b -> [(ek, ev)] -> m (s, [(ek,ev)])
runValidation' v e = (getValidation v) mempty e Nothing >>= \r -> case r of
  (s,es,_) -> return (s,es)

runValidation :: (Monad m) => Validation ek ev s m a b -> [(ek,ev)] -> s -> m (s, [(ek,ev)])
runValidation v e s = (getValidation v) s e Nothing >>= \r -> case r of
  (s,es,_) -> return (s,es)

runValidationId :: Validation ek ev s Identity a b -> s -> (s, [(ek,ev)])
runValidationId v s = runIdentity $ (getValidation v) s [] Nothing >>= \r -> case r of
  (s,es,_) -> return (s,es)


newtype Validation errorKey errorValue state monad a b = 
  Validation { getValidation :: state -> [(errorKey, errorValue)] -> Maybe a -> monad (state, [(errorKey, errorValue)], Maybe b)}


validation :: (Monad m, Eq ek, Eq ev) => Lens b s -> a -> Validator ek ev m a b ->  Validation ek ev s m a b
validation lns a v = Validation $ \s es _ -> case (Just a) of
  Just a -> ((getValidator v) a) >>= \r -> case r of
    Left e  -> return (s, nub (es <> [e]), Nothing)
    Right b -> return (setter lns s b, es, Just b)

validation_ :: (Monad m, Eq ek, Eq ev) => a -> Validator ek ev m a b ->  Validation ek ev s m a b
validation_  a v = Validation $ \s es _ -> case (Just a) of
  Just a -> ((getValidator v) a) >>= \r -> case r of
    Left e  -> return (s, nub (es <> [e]), Nothing)
    Right b -> return (s, es, Just b)


composeValidation :: (Monad m, Eq ek, Eq ev) => Validation ek ev s m a b -> Validation ek ev s m b c -> Validation ek ev s m a c
composeValidation v1 v2 = Validation $ \s es a -> ((getValidation v1) s es a) >>= \r -> case r of
  (s', es', b) -> (getValidation v2) s' (nub (es <> es')) b


instance (Monad m, Eq ek, Eq ev) => Category (Validation ek ev s m) where
  id = Validation (\s es a -> return (s, es, a))
  x . y = composeValidation y x




confirms a b = case (a == b) of
  True -> Right a
  False -> Left "fields do not match."


posted :: (Monad m) => m (View Text, Maybe (Text,Text,Text,Text,Text))
posted = postForm "f" userForm $ testEnv [("f.firstName", "hello"), ("f.lastName", "world"), ("f.email", "hello@world.com"), ("f.emailConfirm", "hello@world.com"), ("f.phoneNumber", "1(333)333-3333x3")]


validateView :: (Monad m, Monoid v, Monoid t) => (s -> Validation Text v t m a b) -> m (View v, Maybe s) -> m (View v, Maybe t)
validateView validation view  = view >>= \(v,x) -> case x of
  Nothing -> return $ (v, Nothing)
  Just s  -> do
    (s,es) <- runValidation' (validation s) []
    es <- return $  (map (\x -> (absolutePath (fst x) v, snd x)) es) <> (viewErrors v)
    case es of
      [] -> return $ (v, Just s)
      _  -> return $ (v {viewErrors = (viewErrors v) <> es}, Nothing)


-- > <*> "smallEvenInteger" .: validate (notEmpty >=> integer >=> conditions [even, greaterThan 0, lessThanOrEq 100]) (text Nothing)
-- > ... -- more fields


testEnv :: Monad m => [(Text, Text)] -> FormEncType -> m (Env m)
testEnv input _formEncType = return $ \key -> return $ map (TextInput . snd) $
    filter ((== fromPath key) . fst) input
