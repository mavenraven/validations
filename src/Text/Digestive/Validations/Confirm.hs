{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Text.Digestive.Validations.Confirm
  ( posted
  , contextValidate
  , userValidation
  ) 
    where

import Prelude hiding ((.))
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Class(lift)
import Text.Digestive.Validations.Internal.Lens(getter, setter, Lens, firstTuple, lens)
import Text.Digestive.Form(Form, validateM, text, (.:))
import Text.Digestive.Types(Result(..))
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

newtype ContextualValidator v s m a = ContextualValidator { getContextValidator :: StateT (View v, s) m a}

instance (Monoid s, Monoid v, Monad m) => Monad (ContextualValidator v s m) where
  return = ContextualValidator . return
  (ContextualValidator st) >>= f = ContextualValidator (st >>= \x -> getContextValidator (f x))

instance MonadTrans (ContextualValidator v s) where
  lift m = ContextualValidator (lift m)


getView :: (Monad m) => ContextualValidator v s m (View v)
getView = ContextualValidator (get >>= \(v,_) -> return v)
putView v = ContextualValidator (get >>= \(_,s) -> put (v,s))

getState :: (Monad m) => ContextualValidator v s m s
getState   = ContextualValidator (get >>= \(_,s) -> return s)
putState s = ContextualValidator (get >>= \(v,_) -> put (v,s))

type FieldName = Text
validation :: (Monad m, Monoid v, Monoid s) => a -> Validator m FieldName v a b -> ContextualValidator v s m (Maybe b)
validation a f = do
  result <- lift $ (getValidator f) a
  case (result) of
    Success x     -> return (Just x)
    Error (fn, e) -> do
      view <- getView
      let path = absolutePath fn view
      let newErrors = (viewErrors view) <> [(path, e)]
      putView $ view {viewErrors = newErrors}
      return Nothing


runValidations :: (Monoid s, Monad m) => View v -> ContextualValidator v s m () -> m (View v, s)
runValidations view cv = runStateT (getContextValidator cv) (view, mempty) >>= \(_,s) -> return s

tupleToType3 :: (a,b,c) -> (a -> b -> c -> d) -> d
tupleToType3 (a,b,c) constructor = constructor a b c


notEmpty x = case x of
  "" -> Error "is empty"
  _  -> Success x



set :: (Monoid s, Monoid v, Monad m) => (forall f. (Functor f) => (a -> f a) -> s -> f s) -> (Maybe a) -> ContextualValidator v s m ()
set lns x = case x of
    Just x' -> do
      s <- getState 
      putState (setter lns s x')
    Nothing -> return ()

attachTo :: (Monad m) => (a -> Result v b) -> fn -> Validator m fn v a b
attachTo validator fieldName = Validator $ \x -> case (validator x) of
  Success x' -> return $ Success x'
  Error  e   -> return $ Error (fieldName, e)

attachMTo :: (Monad m) => (a -> m (Result v b)) -> fn -> Validator m fn v a b
attachMTo validator fieldName = Validator $ \x -> validator x >>= \x -> case x of
  Success x' -> return $ Success x'
  Error  e   -> return $ Error (fieldName, e)

compose :: (Monad m) => (a -> m (Result (k, v) b)) -> (b -> m (Result (k, v) c)) -> (a -> m (Result (k, v) c))
compose f g = (\a -> f a >>= \rb -> case rb of
  Success b     -> g b
  Error   (fn,e) -> return (Error (fn, e)))

data User = User
  { _firstName   :: Text
  , _lastName    :: Text
  , _email       :: Text
  , _phoneNumber :: TPH.PhoneNumber
  } deriving Show

newtype Validator m k v a b  = Validator { getValidator :: a -> m (Result (k, v) b) }

instance (Monad m, Monoid v) => Category (Validator m k v) where
  id = Validator (\x -> return (Success x))
  y . x = Validator (compose (getValidator x) (getValidator y))

instance Monoid User where
  mempty = User { _firstName = "", _lastName = "", _email = "", _phoneNumber = mempty}
  mappend = undefined

firstName :: Lens Text User
firstName = lens _firstName (\s a -> s {_firstName = a})
lastName :: Lens Text User
lastName  = lens _lastName (\s a -> s {_lastName = a})
email :: Lens Text User
email     = lens _email (\s a -> s {_email = a})
phoneNumber :: Lens PhoneNumber User
phoneNumber = lens _phoneNumber (\s a -> s {_phoneNumber = a})


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


confirms a b = case (a == b) of
  True -> Success a
  False -> Error "fields do not match."

coolness x = do
  print "hi"
  return $ Success x


--userValidation :: (Monad m) => (Text,Text,Text,Text,Text) -> ContextualValidator Text User m ()
userValidation (f1,f2,f3,f4,f5) = do
  validation f1 ( 
      notEmpty `attachTo`   firstNameField
      >>>
      notEmpty `attachTo`   firstNameField
      >>>
      coolness `attachMTo`  firstNameField
    )
    >>= set firstName

  validation f2 (
      notEmpty `attachTo` lastNameField
    ) 
    >>= set lastName

  validation f3 (
      notEmpty        `attachTo` emailField
      >>>
      (f4 `confirms`) `attachTo` emailConfirmField
    ) 
    >>= set email

  validation f5 (
      notEmpty                                                         `attachTo` phoneNumberField
      >>>
      ((VPH.phoneNumber  >>> resultMapError (const "bad number"))      `attachTo` phoneNumberField)
      >>> 
      ((VPH.hasExtension >>> resultMapError (const "needs extension")) `attachTo` phoneNumberField)

    ) 
    >>= set phoneNumber



{-
  addValidation helloConfirm x (\(y,z) -> case (z == y) of
    True -> Success (y,z)
    False -> Error "do not match")

    -}



posted :: (Monad m) => m (View Text, Maybe (Text,Text,Text,Text,Text))
posted = postForm "f" userForm $ testEnv [("f.firstName", "hello"), ("f.lastName", "world"), ("f.email", "hello@world.com"), ("f.emailConfirm", "hello@world.com"), ("f.phoneNumber", "1(333)333-3333x3")]


contextValidate :: (Monad m, Monoid v, Monoid s) => m (View v, Maybe a) -> (a -> ContextualValidator v s m ()) -> m (View v, Maybe s)
contextValidate view validator = view >>= \(v,x) -> case x of
  Nothing -> return $ (v, Nothing)
  Just a  -> do
    vs <- runValidations v (validator a)
    case (viewErrors (fst vs)) of
      [] -> return $ (fst vs, Just (snd vs))
      _  -> return $ (fst vs, Nothing)


-- > <*> "smallEvenInteger" .: validate (notEmpty >=> integer >=> conditions [even, greaterThan 0, lessThanOrEq 100]) (text Nothing)
-- > ... -- more fields



--confirm :: (MonadState s m, Eq a) => Lens a s -> Form Text m a -> Form Text m a -> Form Text m a 
--confirm lens fm1 fm2 = validateMForm fm1 fm2 

--confirmForm :: (MonadState Text m) => Form Text m Text
--confirmForm = confirm "" (text Nothing) (text Nothing)
 
--aForm :: (MonadState (Text,Text) m) => Form Text m Text
--aForm = ("brand" .: check "noway!" (\x -> x /= "hi") (text Nothing)) 

testEnv :: Monad m => [(Text, Text)] -> FormEncType -> m (Env m)
testEnv input _formEncType = return $ \key -> return $ map (TextInput . snd) $
    filter ((== fromPath key) . fst) input
