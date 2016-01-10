## Module Toobs.Internal

#### `Proxy`

``` purescript
data Proxy a' a b' b m r
  = Request a' (a -> Proxy a' a b' b m r)
  | Respond b (b' -> Proxy a' a b' b m r)
  | M (m (Proxy a' a b' b m r))
  | Pure r
```

##### Instances
``` purescript
(Functor m) => Functor (Proxy a' a b' b m)
(Functor m) => Apply (Proxy a' a b' b m)
(Functor m) => Applicative (Proxy a' a b' b m)
(Functor m) => Bind (Proxy a' a b' b m)
(Functor m) => Monad (Proxy a' a b' b m)
(Functor m, Semigroup r) => Semigroup (Proxy a' a b' b m r)
(Functor m, Monoid r) => Monoid (Proxy a' a b' b m r)
(MonadPlus m) => Alt (Proxy a' a b' b m)
(MonadPlus m) => Plus (Proxy a' a b' b m)
(MonadPlus m) => Alternative (Proxy a' a b' b m)
(MonadPlus m) => MonadPlus (Proxy a' a b' b m)
MonadTrans (Proxy a' a b' b)
(MonadEff eff m) => MonadEff eff (Proxy a' a b' b m)
(MonadReader r m) => MonadReader r (Proxy a' a b' b m)
(MonadState s m) => MonadState s (Proxy a' a b' b m)
(Monoid w, MonadWriter w m) => MonadWriter w (Proxy a' a b' b m)
(MonadError e m) => MonadError e (Proxy a' a b' b m)
```

#### `X`

``` purescript
newtype X
  = X X
```

#### `closed`

``` purescript
closed :: forall a. X -> a
```


