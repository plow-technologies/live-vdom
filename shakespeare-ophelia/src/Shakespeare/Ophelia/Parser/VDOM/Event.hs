module Shakespeare.Ophelia.Parser.VDOM.Event where

import Control.Monad
import Control.Applicative

import Data.Traversable
import Data.Foldable
import Data.Monoid

-- | An event type that allows for a base event type
-- so that you have a base state for all applications that take an event
data Event a = Unfired  -- ^ The state of an eveent that has never been used
             | Fired a  -- ^ The value of an event.
  deriving (Show, Read, Eq, Ord)

instance Functor Event where
  fmap f (Fired a) = Fired $ f a
  fmap _ Unfired = Unfired

instance Applicative Event where
  pure a = Fired a
  (Fired f) <*> (Fired x) = Fired $ f x
  _ <*> _ = Unfired

instance Alternative Event where
  empty = Unfired
  Unfired <|> f = f
  f <|> _ = f

instance Monad Event where
  (Fired a) >>= f = f a
  Unfired >>= _ = Unfired
  return a = Fired a

instance MonadPlus Event where
  mzero = Unfired
  mplus Unfired x = x
  mplus f@(Fired _) _ = f

instance Foldable Event where
  foldMap f (Fired e) = f e
  foldMap _ _ = mempty
  foldr f b (Fired e) = f e b
  foldr _ b _ = b

instance Traversable Event where
  traverse f (Fired e) = Fired <$> f e
  traverse _ Unfired = pure $ Unfired