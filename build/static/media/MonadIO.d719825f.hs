class (Monad m) => MonadIO m where
  -- | Lift a computation from the 'IO' monad.
  liftIO :: IO a ->  m a

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
