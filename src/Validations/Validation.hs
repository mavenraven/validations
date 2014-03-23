{-# LANGUAGE RankNTypes        #-}

module Validations.Validation
  ( runValidation'
  , runValidation
  , Validation(..)
  , validation
  , validation_
  ) 
    where

import Prelude hiding ((.))
import Validations.Internal.Lens(Lens, setter)
import Data.Monoid(Monoid, (<>), mempty)
import Control.Category(Category(..))
import Data.List(nub)
import Validations.Validator(Validator, getValidator)

runValidation' :: (Monad m, Monoid s) => Validation ek ev s m a b -> [(ek, ev)] -> m (s, [(ek,ev)])
runValidation' v e = (getValidation v) mempty e Nothing >>= \r -> case r of
  (s,es,_) -> return (s,es)

runValidation :: (Monad m) => Validation ek ev s m a b -> [(ek,ev)] -> s -> m (s, [(ek,ev)])
runValidation v e s = (getValidation v) s e Nothing >>= \r -> case r of
  (s',es,_) -> return (s',es)

newtype Validation errorKey errorValue state monad a b = 
  Validation { getValidation :: state -> [(errorKey, errorValue)] -> Maybe a -> monad (state, [(errorKey, errorValue)], Maybe b)}

validation :: (Monad m, Eq ek, Eq ev) => Lens b s -> a -> Validator ek ev m a b ->  Validation ek ev s m a b
validation lns a v = Validation $ \s es _ -> ((getValidator v) a) >>= \r -> case r of
    Left e  -> return (s, nub (es <> [e]), Nothing)
    Right b -> return (setter lns s b, es, Just b)

validation_ :: (Monad m, Eq ek, Eq ev) => a -> Validator ek ev m a b -> Validation ek ev s m a b
validation_  a v = Validation $ \s es _ -> ((getValidator v) a) >>= \r -> case r of
    Left e  -> return (s, nub (es <> [e]), Nothing)
    Right b -> return (s, es, Just b)

composeValidation :: (Monad m, Eq ek, Eq ev) => Validation ek ev s m a b -> Validation ek ev s m b c -> Validation ek ev s m a c
composeValidation v1 v2 = Validation $ \s es a -> ((getValidation v1) s es a) >>= \r -> case r of
  (s', es', b) -> (getValidation v2) s' (nub (es <> es')) b

instance (Monad m, Eq ek, Eq ev) => Category (Validation ek ev s m) where
  id = Validation (\s es a -> return (s, es, a))
  x . y = composeValidation y x
