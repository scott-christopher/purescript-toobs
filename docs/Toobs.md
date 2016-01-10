## Module Toobs

#### `yield`

``` purescript
yield :: forall a m. (Monad m) => a -> Producer a m Unit
```

#### `await`

``` purescript
await :: forall a m. (Monad m) => Consumer a m a
```

#### `cat`

``` purescript
cat :: forall a m r. (Monad m) => Pipe a a m r
```

#### `for`

``` purescript
for :: forall x' x b' b c' c m a'. (Monad m) => Proxy x' x b' b m a' -> (b -> Proxy x' x c' c m b') -> Proxy x' x c' c m a'
```

#### `next`

``` purescript
next :: forall a m r. (Monad m) => Producer a m r -> m (Either r (Tuple a (Producer a m r)))
```

#### `discard`

``` purescript
discard :: forall a m. (Monad m) => a -> m Unit
```

#### `each`

``` purescript
each :: forall a f m. (Monad m, Foldable f) => f a -> Producer a m Unit
```

#### `(~>)`

``` purescript
(~>) :: forall x' x c' c b' b m a' a. (Monad m) => (a -> Proxy x' x b' b m a') -> (b -> Proxy x' x c' c m b') -> a -> Proxy x' x c' c m a'
```

_right-associative / precedence 4_

#### `(<~)`

``` purescript
(<~) :: forall x' x c' c b' b m a' a. (Monad m) => (b -> Proxy x' x c' c m b') -> (a -> Proxy x' x b' b m a') -> a -> Proxy x' x c' c m a'
```

_left-associative / precedence 4_

#### `(>~)`

``` purescript
(>~) :: forall a' a y' y m b c. (Monad m) => Proxy a' a y' y m b -> Proxy Unit b y' y m c -> Proxy a' a y' y m c
```

_right-associative / precedence 5_

#### `(~<)`

``` purescript
(~<) :: forall a' a y' y m b c. (Monad m) => Proxy Unit b y' y m c -> Proxy a' a y' y m b -> Proxy a' a y' y m c
```

_left-associative / precedence 5_

#### `(>->)`

``` purescript
(>->) :: forall a a' b c c' m r. (Monad m) => Proxy a' a Unit b m r -> Proxy Unit b c' c m r -> Proxy a' a c' c m r
```

_left-associative / precedence 7_

#### `(<-<)`

``` purescript
(<-<) :: forall a a' b c c' m r. (Monad m) => Proxy Unit b c' c m r -> Proxy a' a Unit b m r -> Proxy a' a c' c m r
```

_right-associative / precedence 7_


