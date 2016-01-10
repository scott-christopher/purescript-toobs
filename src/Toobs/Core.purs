module Toobs.Core where

import Prelude
import Toobs.Internal

type Effect      = Proxy X Unit Unit X
type Producer  b = Proxy X Unit Unit b
type Pipe    a b = Proxy Unit a Unit b
type Consumer  a = Proxy Unit a Unit X
type Client a' a = Proxy a' a Unit X
type Server b' b = Proxy X Unit b' b

type Effect_      m r = forall x' x y' y. Proxy x' x y' y m r
type Producer_  b m r = forall x' x.      Proxy x' x Unit b m r
type Consumer_  a m r = forall y' y.      Proxy Unit a y' y m r
type Server_ b' b m r = forall x' x.      Proxy x' x b' b m r
type Client_ a' a m r = forall y' y.      Proxy a' a y' y m r

infixl 3 //>
infixr 3 <\\
infixr 4 />/
infixr 4 >\\
infixl 4 \<\
infixl 4 //<
infixl 5 \>\
infixr 5 /</
infixl 6 <<+
infixr 6 +>>
infixl 7 >+>
infixl 7 >>~
infixr 7 <+<
infixr 7 ~<<
infixl 8 <~<
infixr 8 >~>

runEffect :: forall m r. (Monad m) => Effect m r -> m r
runEffect (Request v _) = closed v
runEffect (Respond v _) = closed v
runEffect (M       m)   = m >>= runEffect
runEffect (Pure    r)   = return r

respond :: forall m a a' x x'. (Monad m) => a -> Proxy x' x a' a m a'
respond a = Respond a Pure

(/>/) :: forall m a a' b b' c c' x x'. (Monad m)
    => (a -> Proxy x' x b' b m a')
    -> (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x c' c m a')
(/>/) fa fb a = fa a //> fb

(//>) :: forall m a' b b' c c' x x'. (Monad m)
    =>       Proxy x' x b' b m a'
    -> (b -> Proxy x' x c' c m b')
    ->       Proxy x' x c' c m a'
(//>) (Request x' fx)  fb = Request x' ((//> fb) <<< fx)
(//>) (Respond b  fb') fb = fb b >>= ((//> fb) <<< fb')
(//>) (M m)            fb = M (map (//> fb) m)
(//>) (Pure a)         fb = Pure a

request :: forall m a a' y y'. (Monad m) => a' -> Proxy a' a y' y m a
request a' = Request a' Pure

(\>\) :: forall m a a' b b' c c' y y'. (Monad m)
    => (b' -> Proxy a' a y' y m b)
    -> (c' -> Proxy b' b y' y m c)
    -> (c' -> Proxy a' a y' y m c)
(\>\) fb' fc' c' = fb' >\\ fc' c'

(>\\) :: forall m a a' b b' c y y'. (Monad m)
    => (b' -> Proxy a' a y' y m b)
    ->        Proxy b' b y' y m c
    ->        Proxy a' a y' y m c
(>\\) fb' (Request b' fb)  = fb' b' >>= (fb' >\\) <<< fb
(>\\) fb' (Respond x  fx') = Respond x ((fb' >\\) <<< fx')
(>\\) fb' (M m)            = M (map (fb' >\\) m)
(>\\) fb' (Pure a)         = Pure a

push :: forall a a' m r. (Monad m) => a -> Proxy a' a a' a m r
push a = Respond a (\a' -> Request a' push)

(>~>) :: forall m _a a a' b b' c c' r. (Monad m)
    => (_a -> Proxy a' a b' b m r)
    -> ( b -> Proxy b' b c' c m r)
    -> (_a -> Proxy a' a c' c m r)
(>~>) fa fb a = fa a >>~ fb

(>>~) :: forall m a a' b b' c c' r. (Monad m)
    =>       Proxy a' a b' b m r
    -> (b -> Proxy b' b c' c m r)
    ->       Proxy a' a c' c m r
(>>~) (Request a' fa)  fb = Request a' ((>>~ fb) <<< fa)
(>>~) (Respond b  fb') fb = fb' +>> fb b
(>>~) (M m)            fb = M (map (>>~ fb) m)
(>>~) (Pure r)         fb = Pure r

pull :: forall m a a' r. (Monad m) => a' -> Proxy a' a a' a m r
pull a' = Request a' (\a -> Respond a pull)

(>+>) :: forall m a a' b b' _c' c c' r. (Monad m)
    => ( b' -> Proxy a' a b' b m r)
    -> (_c' -> Proxy b' b c' c m r)
    -> (_c' -> Proxy a' a c' c m r)
(>+>) fb' fc' c' = fb' +>> fc' c'

(+>>) :: forall m a a' b b' c c' r. (Monad m)
    => (b' -> Proxy a' a b' b m r)
    ->        Proxy b' b c' c m r
    ->        Proxy a' a c' c m r
(+>>) fb' (Request b' fb)  = fb' b' >>~ fb
(+>>) fb' (Respond c  fc') = Respond c ((fb' +>>) <<< fc')
(+>>) fb' (M m)            = M (map (fb' +>>) m)
(+>>) fb' (Pure r)         = Pure r

reflect :: forall m a a' b b' r. (Monad m)
    => Proxy a' a b' b m r
    -> Proxy b b' a a' m r
reflect (Request a' fa)  = Respond a' (reflect <<< fa)
reflect (Respond b  fb') = Request b (reflect <<< fb')
reflect (M m)            = M (map reflect m)
reflect (Pure r)         = Pure r

(\<\) :: forall m a a' b b' c c' x x'. (Monad m)
    => (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x b' b m a')
    -> (a -> Proxy x' x c' c m a')
(\<\) p1 p2 = p2 />/ p1

(/</) :: forall m a a' b b' c c' x x'. (Monad m)
    => (c' -> Proxy b' b x' x m c)
    -> (b' -> Proxy a' a x' x m b)
    -> (c' -> Proxy a' a x' x m c)
(/</) p1 p2 = p2 \>\ p1

(<~<) :: forall m a a' b b' c c' r. (Monad m)
    => (b -> Proxy b' b c' c m r)
    -> (a -> Proxy a' a b' b m r)
    -> (a -> Proxy a' a c' c m r)
(<~<) p1 p2 = p2 >~> p1

(<+<) :: forall m a a' b b' c c' r. (Monad m)
    => (c' -> Proxy b' b c' c m r)
    -> (b' -> Proxy a' a b' b m r)
    -> (c' -> Proxy a' a c' c m r)
(<+<) p1 p2 = p2 >+> p1

(<\\) :: forall m a' b b' c c' x x'. (Monad m)
    => (b -> Proxy x' x c' c m b')
    ->       Proxy x' x b' b m a'
    ->       Proxy x' x c' c m a'
(<\\) f p = p //> f

(//<) :: forall m a a' b b' c y y'. (Monad m)
    =>        Proxy b' b y' y m c
    -> (b' -> Proxy a' a y' y m b)
    ->        Proxy a' a y' y m c
(//<) p f = f >\\ p

(~<<) :: forall m a a' b b' c c' r. (Monad m)
    => (b  -> Proxy b' b c' c m r)
    ->        Proxy a' a b' b m r
    ->        Proxy a' a c' c m r
(~<<) k p = p >>~ k

(<<+) :: forall m a a' b b' c c' r. (Monad m)
    =>         Proxy b' b c' c m r
    -> (b'  -> Proxy a' a b' b m r)
    ->         Proxy a' a c' c m r
(<<+) k p = p +>> k
