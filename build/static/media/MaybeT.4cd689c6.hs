data Maybe a = Just a | Nothing
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
