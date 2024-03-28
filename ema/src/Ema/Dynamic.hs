module Ema.Dynamic (
  Dynamic (Dynamic),
) where

import UnliftIO (MonadUnliftIO, concurrently_)

{- | A time-varying value of type `a`, changing under monad `m`.

  To create a `Dynamic`, supply the initial value along with a function that
  forever updates it using the given monadic update function.

 `Dynamic`'s can be composed using `Applicative`.
-}
newtype Dynamic m a
  = Dynamic
      ( -- Initial value
        a
      , -- Set a new value
        (a -> m ()) -> m ()
      )

instance Functor (Dynamic m) where
  fmap f (Dynamic (x0, xf)) =
    Dynamic
      ( f x0
      , \send -> xf $ send . f
      )

instance (MonadUnliftIO m) => Applicative (Dynamic m) where
  pure x = Dynamic (x, const pass)
  liftA2 f (Dynamic (x0, xf)) (Dynamic (y0, yf)) =
    Dynamic
      ( f x0 y0
      , \send -> do
          var <- newTVarIO (x0, y0)
          concurrently_
            ( do
                xf $ \x -> do
                  send <=< atomically $ do
                    modifyTVar' var $ first (const x)
                    f x . snd <$> readTVar var
            )
            ( do
                yf $ \y -> do
                  send <=< atomically $ do
                    modifyTVar' var $ second (const y)
                    (`f` y) . fst <$> readTVar var
            )
      )
