{-# LANGUAGE RankNTypes        #-}

module Validations.Validation
  ( Validation(..)
  , validation
  , validation_
  , composeValidation
  , composeValidation'
  ) 
    where

import Prelude hiding ((.))
import Validations.Internal.Lens(Lens, setter)
import Data.Monoid(Monoid, (<>), mempty)
import Control.Category(Category(..))
import Validations.Validator(Validator(..), runValidator)

newtype Validation errors monad state newState = Validation { runValidation :: state -> monad (newState, errors) } 
 
instance (Monad m, Monoid e) => Category (Validation e m) where
  id = Validation (\s -> return (s,mempty))
  x . y = composeValidation y x 


composeValidation' :: (Monad m) => (e -> f -> g) -> Validation e m s t -> Validation f m t u -> Validation g m s u
composeValidation' fn (Validation v1) (Validation v2) = 
  Validation $ \s      -> v1 s >>=
               \(t,e)  -> v2 t >>=
               \(u,f) -> return (u, fn e f)

composeValidation :: (Monad m, Monoid e) => Validation e m s t -> Validation e m t u -> Validation e m s u
composeValidation = composeValidation' (<>)

validation :: (Monad m) => Lens b s -> a -> Validator ek ev m a b -> Validation [(ek,ev)] m s s
validation lens a (Validator v) =
  Validation $ \s -> v a >>= 
  \r -> case r of
    Left e  -> return (s, [e])
    Right b -> return (setter lens s b, [])

validation_ :: (Monad m) =>  a -> Validator ek ev m a b -> Validation [(ek,ev)] m s s
validation_ a (Validator v) =
  Validation $ \s -> v a >>= 
  \r -> case r of
    Left e  -> return (s, [e])
    Right b -> return (s, [])
