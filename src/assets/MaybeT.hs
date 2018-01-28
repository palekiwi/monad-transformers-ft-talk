newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }
