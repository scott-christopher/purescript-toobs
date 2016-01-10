module Toobs.Enumerable where

import Prelude
import Control.Monad.Except.Trans
import Control.Monad.Maybe.Trans
import Data.Either
import Data.Maybe

import Toobs
import Toobs.Core
import Toobs.ListT


every :: forall a m t. (Monad m, Enumerable t) => t m a -> Producer a m Unit
every it = discard >\\ enumerate (toListT it)

class Enumerable t where
  toListT :: forall a m. (Monad m) => t m a -> ListT m a

instance listTEnumerable :: Enumerable ListT where
  toListT = id

instance maybeTEnumerable :: Enumerable MaybeT where
  toListT m = Select $ do x <- lift (runMaybeT m)
                          case x of Nothing -> return unit
                                    Just a  -> yield a

instance exceptTEnumerable :: Enumerable (ExceptT e) where
  toListT m = Select $ do x <- lift (runExceptT m)
                          case x of Left  _ -> return unit
                                    Right a -> yield a

