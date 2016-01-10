module Toobs where

import Prelude
import Data.Foldable
import Data.Either
import Data.Tuple

import Toobs.Core
import Toobs.Internal


infixl 4 <~
infixr 4 ~>
infixl 5 ~<
infixr 5 >~
infixl 7 >->
infixr 7 <-<

yield :: forall a m. (Monad m) => a -> Producer a m Unit
yield = respond

await :: forall a m. (Monad m) => Consumer a m a
await = request unit

cat :: forall a m r. (Monad m) => Pipe a a m r
cat = pull unit

for :: forall x' x b' b c' c m a'. (Monad m)
  =>       Proxy x' x b' b m a'
  -> (b -> Proxy x' x c' c m b')
  ->       Proxy x' x c' c m a'
for = (//>)

next :: forall a m r. (Monad m)
  => Producer a m r
  -> m (Either r (Tuple a (Producer a m r)))
next (Request v _)  = closed v
next (Respond a fu) = return (Right (Tuple a (fu unit)))
next (M m)          = m >>= next
next (Pure r)       = return (Left r)

discard :: forall a m. (Monad m) => a -> m Unit
discard _ = return unit

each :: forall a f m. (Monad m, Foldable f) => f a -> Producer a m Unit
each = traverse_ yield

(~>) :: forall x' x c' c b' b m a' a. (Monad m)
  => (a -> Proxy x' x b' b m a')
  -> (b -> Proxy x' x c' c m b')
  -> (a -> Proxy x' x c' c m a')
(~>) = (/>/)

(<~) :: forall x' x c' c b' b m a' a. (Monad m)
  => (b -> Proxy x' x c' c m b')
  -> (a -> Proxy x' x b' b m a')
  -> (a -> Proxy x' x c' c m a')
(<~) g f = f ~> g

(>~) :: forall a' a y' y m b c. (Monad m)
  => Proxy a'   a y' y m b
  -> Proxy Unit b y' y m c
  -> Proxy a'   a y' y m c
(>~) p1 p2 = const p1 >\\ p2

(~<) :: forall a' a y' y m b c. (Monad m)
  => Proxy Unit b y' y m c
  -> Proxy a'   a y' y m b
  -> Proxy a'   a y' y m c
(~<) p2 p1 = p1 >~ p2

(>->) :: forall a a' b c c' m r. (Monad m)
  => Proxy a'   a Unit b m r
  -> Proxy Unit b c'   c m r
  -> Proxy a'   a c'   c m r
(>->) p1 p2 = const p1 +>> p2

(<-<) :: forall a a' b c c' m r. (Monad m)
  => Proxy Unit b c'   c m r
  -> Proxy a'   a Unit b m r
  -> Proxy a'   a c'   c m r
(<-<) p2 p1 = p1 >-> p2

