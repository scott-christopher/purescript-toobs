## Module Toobs.Enumerable

#### `every`

``` purescript
every :: forall a m t. (Monad m, Enumerable t) => t m a -> Producer a m Unit
```

#### `Enumerable`

``` purescript
class Enumerable t where
  toListT :: forall a m. (Monad m) => t m a -> ListT m a
```

##### Instances
``` purescript
Enumerable ListT
Enumerable MaybeT
Enumerable (ExceptT e)
```


