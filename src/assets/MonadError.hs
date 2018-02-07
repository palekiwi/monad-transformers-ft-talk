class (Monad m) => MonadError e m | m -> e where
    -- | Is used within a monadic computation to begin exception processing.
    throwError :: e -> m a

    -- do { action1; action2; action3 } `catchError` handler
    catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r
