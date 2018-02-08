newtype State s a = State {runState :: s -> (a, s)}
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
