newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }
