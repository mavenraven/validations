{-# LANGUAGE Rank2Types #-}

module Text.Digestive.Validations.Internal.Lens
  ( getter
  , setter
  , lens
  , firstTuple
  , secondTuple
  , Lens
  , Lens'
  ) where

import Control.Monad.Identity(Identity(Identity), runIdentity)
import Control.Applicative(Const(Const), getConst, (<$>))

type Lens a s = (Functor f) => (a -> f a) -> s -> f s
type Lens' f a s = (a -> f a) -> s -> f s

getter :: (forall f. (Functor f) => (a -> f a) -> s -> f s) -> s -> a
getter lns x = getConst $ lns (\y -> Const y) x

setter :: (forall f. (Functor f) => (a -> f a) -> s -> f s) -> s -> a -> s
setter lns x y = runIdentity $ lns (\_ -> Identity y) x

lens :: (Functor f) => (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s 
lens get set lift input = set input <$> (lift . get) input





firstTuple :: Lens a (a,b)
firstTuple = lens fst (\x y -> (y, snd x))


secondTuple :: Lens b (a,b)
secondTuple = lens snd (\x y -> (fst x, y))

