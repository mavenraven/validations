{-# LANGUAGE RankNTypes        #-}

module Validations.Validation
  ( Validation(..)
  , validation
  ) 
    where

import Prelude hiding ((.))
import Validations.Internal.Lens(Lens, setter)
import Data.Monoid(Monoid, (<>), mempty)
import Control.Category(Category(..))
import Validations.Validator(Validator, runValidator)

newtype Validation errors monad state newState = Validation { runValidation :: state -> monad (newState, errors) } 
 
instance (Monad m, Monoid e) => Category (Validation e m) where
  id = Validation (\s -> return (s,mempty))
  x . y = composeValidation y x 

composeValidation :: (Monad m, Monoid e) => Validation e m s t -> Validation e m t u -> Validation e m s u
composeValidation v1 v2 = Validation $ \s      -> ((runValidation v1) s) >>=
                                       \(t,e)  -> ((runValidation v2) t) >>=
                                       \(u,e') -> return (u, e <> e')

validation :: (Monad m) => Lens b s -> a -> Validator ek ev m a b ->  Validation [(ek,ev)] m s s
validation lns a v = Validation $ \s -> ((runValidator v) a) >>= \r -> case r of
    Left e  -> return (s, [e])
    Right b -> return (setter lns s b, [])
