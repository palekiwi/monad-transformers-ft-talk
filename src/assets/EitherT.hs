newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }
