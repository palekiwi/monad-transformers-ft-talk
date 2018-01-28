instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

lift                  :: Monad m => m a -> t m a
(MaybeT . liftM Just) :: Monad m => m a -> MaybeT m a

MaybeT                :: m (Maybe a) -> MaybeT m a
(liftM Just)          :: Monad m => m a -> m (Maybe a)
