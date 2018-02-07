data Either e a = Left e | Right a
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
