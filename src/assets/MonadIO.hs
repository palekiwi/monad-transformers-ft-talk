class (Monad m) => MonadIO m where
  -- | Lift a computation froh the 'IO' monad.
  liftIO :: IO a ->  m a
