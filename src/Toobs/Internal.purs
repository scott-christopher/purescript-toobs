module Toobs.Internal where

import Prelude
import Control.Alt
import Control.Alternative
import Control.Monad.Eff.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Control.MonadPlus
import Control.Plus
import Data.Monoid
import Data.Tuple


data Proxy a' a b' b m r = Request a' (a  -> Proxy a' a b' b m r)
                         | Respond b  (b' -> Proxy a' a b' b m r)
                         | M          (m    (Proxy a' a b' b m r))
                         | Pure    r

instance proxyFunctor :: (Functor m) => Functor (Proxy a' a b' b m) where
  map f (Request x g) = Request x (map f <<< g)
  map f (Respond x g) = Respond x (map f <<< g)
  map f (M x)         = M (map (map f) x)
  map f (Pure r)      = Pure (f r)

instance proxyApply :: (Functor m) => Apply (Proxy a' a b' b m) where
  apply = ap

instance proxyApplicative :: (Functor m) => Applicative (Proxy a' a b' b m) where
  pure = Pure

instance proxyBind :: (Functor m) => Bind (Proxy a' a b' b m) where
  bind (Request x f) g = Request x ((>>= g) <<< f)
  bind (Respond x f) g = Respond x ((>>= g) <<< f)
  bind (M x)         g = M (map (>>= g) x)
  bind (Pure x)      g = g x

instance proxyMonad :: (Functor m) => Monad (Proxy a' a b' b m)

instance proxySemigroup :: (Functor m, Semigroup r) => Semigroup (Proxy a' a b' b m r) where
  append (Request x f) p = Request x ((`append` p) <<< f)
  append (Respond x f) p = Respond x ((`append` p) <<< f)
  append (M x)         p = M (map (`append` p) x)
  append (Pure r)      p = map (append r) p

instance proxyMonoid :: (Functor m, Monoid r) => Monoid (Proxy a' a b' b m r) where
  mempty = Pure mempty

instance proxyAlt :: (MonadPlus m) => Alt (Proxy a' a b' b m) where
  alt (Request a' fa) p = Request a' (\a  -> (fa a) <|> p)
  alt (Respond b fb') p = Respond b  (\b' -> (fb' b') <|> p)
  alt (Pure r)        p = Pure r
  alt (M m)           p = M ((do p' <- m
                                 return (p' <|> p)) <|> return p)

instance proxyPlus :: (MonadPlus m) => Plus (Proxy a' a b' b m) where
  empty = lift empty

instance proxyAlternative :: (MonadPlus m) => Alternative (Proxy a' a b' b m)

instance proxyMonadPlus :: (MonadPlus m) => MonadPlus (Proxy a' a b' b m)

instance proxyMonadTrans :: MonadTrans (Proxy a' a b' b) where
  lift m = M (map Pure m)

instance proxyMonadEff :: (MonadEff eff m) => MonadEff eff (Proxy a' a b' b m) where
  liftEff m = M (liftEff (map Pure m))

instance proxyMonadReader :: (MonadReader r m) => MonadReader r (Proxy a' a b' b m) where
  ask = lift ask
  local f (Request a' fa) = Request a' (local f <<< fa)
  local f (Respond b fb') = Respond b (local f <<< fb')
  local f (Pure r)        = Pure r
  local f (M m)           = M (local f (map (local f) m))

instance proxyMonadState :: (MonadState s m) => MonadState s (Proxy a' a b' b m) where
  state = lift <<< state

instance proxyMonadWriter :: (Monoid w, MonadWriter w m) => MonadWriter w (Proxy a' a b' b m) where
  writer = lift <<< writer

  listen p0 = go p0 mempty
    where
      go p w = case p of
        Request a' fa -> Request a' (\a  -> go (fa  a ) w)
        Respond b fb' -> Respond b  (\b' -> go (fb' b') w)
        Pure r        -> Pure (Tuple r w)
        M m           -> M (do Tuple p' w' <- listen m
                               return (go p' (append w w')))

  pass p0 = go p0 mempty
    where
      go p w = case p of
        Request a' fa    -> Request a' (\a  -> go (fa  a ) w)
        Respond b fb'    -> Respond b  (\b' -> go (fb' b') w)
        Pure (Tuple r f) -> M (pass (return (Tuple (Pure r) \_ -> f w)))
        M m              -> M (do Tuple p' w' <- listen m
                                  return (go p' (append w w')))

instance proxyMonadError :: (MonadError e m) => MonadError e (Proxy a' a b' b m) where
  throwError = lift <<< throwError

  catchError (Request a' fa) f = Request a' (\a  -> catchError (fa  a ) f)
  catchError (Respond b fb') f = Respond b  (\b' -> catchError (fb' b') f)
  catchError (Pure r)        f = Pure r
  catchError (M m)           f = M ((do p' <- m
                                        return (catchError p' f)) `catchError` (return <<< f))

newtype X = X X

closed :: forall a. X -> a
closed (X x) = closed x

