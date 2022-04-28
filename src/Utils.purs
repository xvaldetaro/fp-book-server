module Utils where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (class MonadTrans, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar

liftEither :: ∀ e a m. MonadError e m => Either e a -> m a
liftEither (Left x) = throwError x
liftEither (Right x) = pure x

liftSuccess
  :: ∀ e a m t
  .Monad m
  => MonadTrans t
  => MonadError e (t m)
  => m (Either e a)
  -> t m a
liftSuccess x = lift x >>= liftEither

withAVar :: ∀ a b. AVar a -> (a -> Aff (Tuple a b)) -> Aff b
withAVar avar f = do
  x <- AVar.take avar
  Tuple newX result <- f x
  AVar.put newX avar
  pure result
