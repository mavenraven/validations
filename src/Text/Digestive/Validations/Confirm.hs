{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts        #-}


module Text.Digestive.Validations.Confirm
--  ( phoneNumber
--  , hasAreaCode
--  , hasExtension
--  , hasCountryCode
--  ) 
    where

import Control.Monad.State(MonadState, put, get, State, runState)
import Text.Digestive.Validations.Internal.Lens(getter, setter, Lens, firstTuple)
import Text.Digestive.Form(Form, validateM, text, (.:))
import Text.Digestive.Types(Result(..))
import Text.Digestive.Form
import Text.Digestive.Form.Encoding
import Text.Digestive.Types
import Control.Applicative
import Data.Text(Text)
import Debug.Trace
import Control.Monad
import Data.Monoid
import Text.Digestive.View
--confirm :: (Monad m, Eq a) => Form Text m a -> Form Text m a -> Form Text m a
--confirm fm1 fm2 = (flip runState) False $
--
id :: a -> a
id = undefined
 


--needsConfirmation :: (MonadState s m) => ((s,a) -> s) -> Form T.Text m a -> Form T.Text m a
needsConfirmation f = validateM g 
 where g a = do {
    s <- get;
    put (f s a);
    return (Error "no");
 }

--confirms :: (MonadState s m, Eq a) => (s -> a) -> Form T.Text m a -> Form T.Text m a 
confirmsFor f = validateM g
  where g a = do {
    s <- get;
    if ((f s) == a)
       then return $ Success a
       else return $ Error "not equal!"
   }


validateMForm :: (MonadState a m, Eq a) => Form Text m a -> Form Text m a -> Form Text m a
validateMForm fm1 fm2 = ("hello" .: (flip validateM) fm1 (\x -> do
                        put x
                        return $ Success (x)))
                     *> ("bye" .: (flip validateM) fm2 (\x -> do 
                      s <- get
                      if (x /= s)
                         then return $ Error "bye"
                         else return $ Success (x)))




newtype ContextualValidator v a = ContextualValidator { getContextValidator :: State (View v) a}

instance (Monoid v) => Monad (ContextualValidator v) where
  return = ContextualValidator . return
  (ContextualValidator st) >>= f = ContextualValidator (st >>= \x -> getContextValidator (f x))


getView = ContextualValidator  get
putView v = ContextualValidator (put v)


type FieldName = Text
validation :: (Monoid e) => a -> (a -> Result (FieldName, e) b) -> ContextualValidator e ()
validation a f = case (f a) of
  Success x -> return ()
  Error   (fn, e) -> do
    view <- getView
    let path = absolutePath fn view
    let newErrors = (viewErrors view) <> [(path, e)]
    putView $ view {viewErrors = newErrors}


runValidations :: View v -> ContextualValidator v () -> View v
runValidations view cv = snd $ runState (getContextValidator cv) view

tupleToType3 :: (a,b,c) -> (a -> b -> c -> d) -> d
tupleToType3 (a,b,c) constructor = constructor a b c


notEmpty x = case x of
  "" -> Error "is empty"
  _  -> Success x

hello = "hello"
helloConfirm = "helloConfirm"
goodbye = "goodbye"


attachTo validator fieldName = \x -> case (validator x) of
  Success x' -> Success x'
  Error  e   -> Error (fieldName, e)

coolForm :: (Monad m) => Form Text m (Text,Text,Text)
coolForm = (,,)
  <$> hello        .: (text Nothing)
  <*> helloConfirm .: (text Nothing)
  <*> goodbye      .: (text Nothing)

confirms a b = case (a == b) of
  True -> Success a
  False -> Error "fields do not match."

coolValidator (f1,f2,f3) = do
  validation f1 $
    notEmpty      `attachTo` hello
    >=>
    (confirms f2) `attachTo` helloConfirm

  validation f3 $
    notEmpty      `attachTo` goodbye
    >=>
    (confirms f1) `attachTo` goodbye




{-
  addValidation helloConfirm x (\(y,z) -> case (z == y) of
    True -> Success (y,z)
    False -> Error "do not match")
    -}



posted :: (Monad m) => m (View Text, Maybe (Text,Text,Text))
posted = postForm "f" coolForm $ testEnv [("f.hello", "hiz"), ("f.helloConfirm", "hi"), ("f.goodbye", "hi")]


contextValidate :: (Monad m, Monoid v) => m (View v, Maybe a) -> (a -> ContextualValidator v ()) -> m (View v, Maybe a)
contextValidate view validator = view >>= \(v,x) -> case x of
  Nothing -> return $ (v,x)
  Just a  -> return (runValidations v (validator a), x)


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
