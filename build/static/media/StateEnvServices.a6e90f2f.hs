class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s

    -- | Replace the state inside the monad.
    put :: s -> m ()

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a

-- | Maps an old state to a new state inside a state monad.
-- The old state is thrown away.
modify :: MonadState s m => (s -> s) -> m ()
