{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Arrow qualified as Arrow
import Control.Category ((>>>))
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse, firstA)
import Data.Either (either)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Const (Const (..))
import Data.Tuple (swap)
import System.IO (print)
import Prelude
  ( Applicative (..),
    Either (..),
    Functor (..),
    IO,
    Num (..),
    Traversable (..),
    const,
    fst,
    snd,
  )

class (forall a. Functor (p a)) => Profunctor p where
  lmap :: (b -> a) -> p a c -> p b c

dimap :: (Profunctor p) => (c -> a) -> (b -> d) -> p a b -> p c d
dimap f g = lmap f >>> fmap g

class (Profunctor p) => Strong p where
  first' :: p a b -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)

class (Profunctor p) => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

-- ProductProfunctor?
class (Profunctor p) => Monoidal p where
  par :: p a b -> p c d -> p (a, c) (b, d)
  empty :: p () ()

instance Profunctor (->) where
  lmap = (>>>)

instance Strong (->) where
  first' = first
  second' = second

instance Choice (->) where
  left' f = either (f >>> Left) Right
  right' f = either Left (f >>> Right)

instance Monoidal (->) where
  par = (Arrow.***)
  empty () = ()

newtype Star f a b = Star {runStar :: a -> f b}
  deriving (Functor)

instance (Functor f) => Profunctor (Star f) where
  lmap f = runStar >>> (f >>>) >>> Star

instance (Functor f) => Strong (Star f) where
  first' (Star f) = Star (\(a, b) -> f a <&> (,b))
  second' (Star f) = Star (\(a, b) -> f b <&> (a,))

instance (Applicative f) => Choice (Star f) where
  left' = runStar >>> firstA >>> Star
  right' = runStar >>> traverse >>> Star

instance (Applicative f) => Monoidal (Star f) where
  empty = Star pure
  par (Star f) (Star g) = Star (bitraverse f g)

newtype Costar f a b = Costar {runCostar :: f a -> b}
  deriving (Functor)

instance (Functor f) => Profunctor (Costar f) where
  lmap f = runCostar >>> lmap (fmap f) >>> Costar

instance (Functor f) => Monoidal (Costar f) where
  empty = Costar (const ())
  par (Costar f) (Costar g) = Costar (\fx -> (fx <&> fst & f, fx <&> snd & g))

newtype Tagged a b = Tagged {unTagged :: b}
  deriving (Functor)

instance Profunctor Tagged where
  lmap _f = unTagged >>> Tagged

instance Choice Tagged where
  left' = unTagged >>> Left >>> Tagged
  right' = unTagged >>> Right >>> Tagged

instance Monoidal Tagged where
  empty = Tagged ()
  par (Tagged a) (Tagged b) = Tagged (a, b)

type Iso s t a b = forall p. (Profunctor p) => p a b -> p s t

type Iso' s a = Iso s s a a

type Lens s t a b = forall p. (Strong p) => p a b -> p s t

type Lens' s a = Lens s s a a

type Prism s t a b = forall p. (Choice p) => p a b -> p s t

type Prism' s a = Prism s s a a

re :: Iso s t a b -> Iso b a t s
re o = dimap (review o) (view o)

view :: Lens s t a b -> s -> a
view o = runStar (o (Star Const)) >>> getConst

review :: Prism s t a b -> b -> t
review o = Tagged >>> o >>> unTagged

over :: Lens s t a b -> (a -> b) -> s -> t
over o = o

testAssoc :: Iso' (a, (b, c)) ((a, b), c)
testAssoc =
  dimap
    (\(a, (b, c)) -> ((a, b), c))
    (\((a, b), c) -> (a, (b, c)))

testComm :: Iso (a, b) (a', b') (b, a) (b', a')
testComm = dimap swap swap

_1 :: Lens (a, b) (a', b) a a'
_1 = first'

-- Testing composition
_2 :: Lens (a, b) (a, b') b b'
_2 = _1 >>> testComm

main :: IO ()
main = do
  view testAssoc (1, (2, 3)) & print
  review testAssoc ((1, 2), 3) & print

  view _1 (1, 2) & print
  over _1 (+ 2) (1, 2) & print

  view _2 (1, 2) & print
  over _2 (+ 1) (1, 2) & print
