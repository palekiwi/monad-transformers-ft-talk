newtype Reader r a = Reader { runReader :: r -> a }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
