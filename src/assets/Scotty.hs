newtype ScottyT e m a =
  ScottyT { runS :: State (ScottyState e m) a }
  deriving ( Functor, Applicative, Monad )

newtype ActionT e m a =
  ActionT { runAM :: ExceptT (ActionError e)
                             (ReaderT ActionEnv
                             (StateT ScottyResponse m)) a }
  deriving ( Functor, Applicative )
