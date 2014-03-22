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

newtype Validation errorKey errorValue state monad a = 
  Validation { getValidation :: StateT ([(errorKey, errorValue)], state) monad a}

instance (Monad m) => Monad (Validation ek ev s m) where
  return = Validation . return
  (Validation st) >>= f = Validation (st >>= \x -> getValidation (f x))

instance MonadTrans (Validation ek ev s) where
  lift m = Validation (lift m)


getErrors :: (Monad m) => Validation ek ev s m [(ek, ev)]
getErrors  = Validation (get >>= \(es,_) -> return es)

addError :: (Monad m) => (ek, ev) -> Validation ek ev s m ()
addError e = Validation (get >>= \(es,s) -> put (es <> [e],s))

getState :: (Monad m) => Validation ek ev s m s
getState  = Validation (get >>= \(_,s) -> return s)

putState  :: (Monad m) => s -> Validation ek ev s m ()
putState s = Validation (get >>= \(es,_) -> put (es,s))

validation :: (Monad m) => a -> Validator ek ev m a b -> Validation ek ev s m (Maybe b)
validation a f = do
  result <- lift $ (getValidator f) a
  case (result) of
    Right x      -> return (Just x)
    Left (ek, ev) -> do
      addError (ek,ev)
      return Nothing


runValidation :: (Monad m) => Validation ek ev s m a -> s -> m ([(ek,ev)], s)
runValidation v s = runStateT (getValidation v) ([], s)  >>= return . snd 


notEmpty x = case x of
  "" -> Left "is empty"
  _  -> Right x



set :: (Monad m) => (forall f. (Functor f) => (a -> f a) -> s -> f s) -> (Maybe a) -> Validation errorKey errorValue s m ()
set lns x = case x of
    Just x' -> do
      s <- getState 
      putState (setter lns s x')
    Nothing -> return ()


type Checker error a b = a -> Either error b
type MonadicChecker error monad a b = a -> monad (Either error b)

attachTo :: (Monad m) => Checker ev a b -> ek -> Validator ek ev m a b
attachTo validator fieldName = Validator $ \x -> case (validator x) of
  Right x' -> return $ Right x'
  Left  e   -> return $ Left (fieldName, e)

attachMTo :: (Monad m) => MonadicChecker ev m a b -> ek -> Validator ek ev m a b
attachMTo validator fieldName = Validator $ \x -> validator x >>= \x -> case x of
  Right x' -> return $ Right x'
  Left  e   -> return $ Left (fieldName, e)

compose :: (Monad m) => (a -> m (Either (k, v) b)) -> (b -> m (Either (k, v) c)) -> (a -> m (Either (k, v) c))
compose f g = (\a -> f a >>= \rb -> case rb of
  Right b     -> g b
  Left   (fn,e) -> return (Left (fn, e)))

data User = User
  { _firstName   :: Text
  , _lastName    :: Text
  , _email       :: Text
  , _phoneNumber :: TPH.PhoneNumber
  } deriving Show

newtype Validator errorKey errorValue monad a b  = Validator { getValidator :: a -> monad (Either (errorKey, errorValue) b) }

instance (Monad m) => Category (Validator ek ev m) where
  id = Validator (\x -> return (Right x))
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
phoneNumber2 :: Lens PhoneNumber User
phoneNumber2 = lens _phoneNumber (\s a -> s {_phoneNumber = a})


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
  True -> Left a
  False -> Right "fields do not match."

coolness x = do
  print "hi"
  return $ Right x


--userValidation :: (Monad m) => (Text,Text,Text,Text,Text) -> Validation Text User m ()
userValidation (f1,f2,f3,f4,f5) = do
  validation f1 ( 
      notEmpty `attachTo`   firstNameField
      >>>
      notEmpty `attachTo`   firstNameField
      >>>
      coolness `attachMTo`  firstNameField
    )
    >>= set firstName
{-

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


contextValidate :: (Monad m, Monoid v, Monoid s) => m (View v, Maybe a) -> (a -> Validation v s m ()) -> m (View v, Maybe s)
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
-}
