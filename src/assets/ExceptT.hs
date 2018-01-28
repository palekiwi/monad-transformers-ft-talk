newtype ExceptT e m a =
  ExceptT { runEitherT :: m (Either e a) }
