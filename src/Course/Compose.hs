{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Optional
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a)) deriving (Show, Eq)

liftTwice :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
liftTwice = (<$>) . (<$>)

-- ap :: f (a -> b) -> f a -> f b
-- <$> :: (a -> b) -> f a -> f b
--- h :: f (g (a -> b))

 -- ap <$> h ::  f (g a -> g b) -> f (g b)

liftATwice :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
liftATwice h fa = (<*>) <$> h <*> fa

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  h <$> (Compose fga) = Compose $ liftTwice h fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a = Compose (pure . pure $ a)
-- Implement the (<*>) function for an Applicative instance for Compose
  (Compose fgfunc) <*> (Compose fga) = Compose $ liftATwice fgfunc fga


instance (Monad f, Monad g) => Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) = error "todo: Course.Compose (<<=)#instance (Compose f g)"
