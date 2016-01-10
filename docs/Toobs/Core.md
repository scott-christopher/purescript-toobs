## Module Toobs.Core

#### `Effect`

``` purescript
type Effect = Proxy X Unit Unit X
```

#### `Producer`

``` purescript
type Producer b = Proxy X Unit Unit b
```

#### `Pipe`

``` purescript
type Pipe a b = Proxy Unit a Unit b
```

#### `Consumer`

``` purescript
type Consumer a = Proxy Unit a Unit X
```

#### `Client`

``` purescript
type Client a' a = Proxy a' a Unit X
```

#### `Server`

``` purescript
type Server b' b = Proxy X Unit b' b
```

#### `Effect_`

``` purescript
type Effect_ m r = forall x' x y' y. Proxy x' x y' y m r
```

#### `Producer_`

``` purescript
type Producer_ b m r = forall x' x. Proxy x' x Unit b m r
```

#### `Consumer_`

``` purescript
type Consumer_ a m r = forall y' y. Proxy Unit a y' y m r
```

#### `Server_`

``` purescript
type Server_ b' b m r = forall x' x. Proxy x' x b' b m r
```

#### `Client_`

``` purescript
type Client_ a' a m r = forall y' y. Proxy a' a y' y m r
```

#### `runEffect`

``` purescript
runEffect :: forall m r. (Monad m) => Effect m r -> m r
```

#### `respond`

``` purescript
respond :: forall m a a' x x'. (Monad m) => a -> Proxy x' x a' a m a'
```

#### `(/>/)`

``` purescript
(/>/) :: forall m a a' b b' c c' x x'. (Monad m) => (a -> Proxy x' x b' b m a') -> (b -> Proxy x' x c' c m b') -> a -> Proxy x' x c' c m a'
```

_right-associative / precedence 4_

#### `(//>)`

``` purescript
(//>) :: forall m a' b b' c c' x x'. (Monad m) => Proxy x' x b' b m a' -> (b -> Proxy x' x c' c m b') -> Proxy x' x c' c m a'
```

_left-associative / precedence 3_

#### `request`

``` purescript
request :: forall m a a' y y'. (Monad m) => a' -> Proxy a' a y' y m a
```

#### `(\>\)`

``` purescript
(\>\) :: forall m a a' b b' c c' y y'. (Monad m) => (b' -> Proxy a' a y' y m b) -> (c' -> Proxy b' b y' y m c) -> c' -> Proxy a' a y' y m c
```

_left-associative / precedence 5_

#### `(>\\)`

``` purescript
(>\\) :: forall m a a' b b' c y y'. (Monad m) => (b' -> Proxy a' a y' y m b) -> Proxy b' b y' y m c -> Proxy a' a y' y m c
```

_right-associative / precedence 4_

#### `push`

``` purescript
push :: forall a a' m r. (Monad m) => a -> Proxy a' a a' a m r
```

#### `(>~>)`

``` purescript
(>~>) :: forall m _a a a' b b' c c' r. (Monad m) => (_a -> Proxy a' a b' b m r) -> (b -> Proxy b' b c' c m r) -> _a -> Proxy a' a c' c m r
```

_right-associative / precedence 8_

#### `(>>~)`

``` purescript
(>>~) :: forall m a a' b b' c c' r. (Monad m) => Proxy a' a b' b m r -> (b -> Proxy b' b c' c m r) -> Proxy a' a c' c m r
```

_left-associative / precedence 7_

#### `pull`

``` purescript
pull :: forall m a a' r. (Monad m) => a' -> Proxy a' a a' a m r
```

#### `(>+>)`

``` purescript
(>+>) :: forall m a a' b b' _c' c c' r. (Monad m) => (b' -> Proxy a' a b' b m r) -> (_c' -> Proxy b' b c' c m r) -> _c' -> Proxy a' a c' c m r
```

_left-associative / precedence 7_

#### `(+>>)`

``` purescript
(+>>) :: forall m a a' b b' c c' r. (Monad m) => (b' -> Proxy a' a b' b m r) -> Proxy b' b c' c m r -> Proxy a' a c' c m r
```

_right-associative / precedence 6_

#### `reflect`

``` purescript
reflect :: forall m a a' b b' r. (Monad m) => Proxy a' a b' b m r -> Proxy b b' a a' m r
```

#### `(\<\)`

``` purescript
(\<\) :: forall m a a' b b' c c' x x'. (Monad m) => (b -> Proxy x' x c' c m b') -> (a -> Proxy x' x b' b m a') -> a -> Proxy x' x c' c m a'
```

_left-associative / precedence 4_

#### `(/</)`

``` purescript
(/</) :: forall m a a' b b' c c' x x'. (Monad m) => (c' -> Proxy b' b x' x m c) -> (b' -> Proxy a' a x' x m b) -> c' -> Proxy a' a x' x m c
```

_right-associative / precedence 5_

#### `(<~<)`

``` purescript
(<~<) :: forall m a a' b b' c c' r. (Monad m) => (b -> Proxy b' b c' c m r) -> (a -> Proxy a' a b' b m r) -> a -> Proxy a' a c' c m r
```

_left-associative / precedence 8_

#### `(<+<)`

``` purescript
(<+<) :: forall m a a' b b' c c' r. (Monad m) => (c' -> Proxy b' b c' c m r) -> (b' -> Proxy a' a b' b m r) -> c' -> Proxy a' a c' c m r
```

_right-associative / precedence 7_

#### `(<\\)`

``` purescript
(<\\) :: forall m a' b b' c c' x x'. (Monad m) => (b -> Proxy x' x c' c m b') -> Proxy x' x b' b m a' -> Proxy x' x c' c m a'
```

_right-associative / precedence 3_

#### `(//<)`

``` purescript
(//<) :: forall m a a' b b' c y y'. (Monad m) => Proxy b' b y' y m c -> (b' -> Proxy a' a y' y m b) -> Proxy a' a y' y m c
```

_left-associative / precedence 4_

#### `(~<<)`

``` purescript
(~<<) :: forall m a a' b b' c c' r. (Monad m) => (b -> Proxy b' b c' c m r) -> Proxy a' a b' b m r -> Proxy a' a c' c m r
```

_right-associative / precedence 7_

#### `(<<+)`

``` purescript
(<<+) :: forall m a a' b b' c c' r. (Monad m) => Proxy b' b c' c m r -> (b' -> Proxy a' a b' b m r) -> Proxy a' a c' c m r
```

_left-associative / precedence 6_


