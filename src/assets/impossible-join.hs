join :: (Monad m) => m (m a) -> m a

-- this is impossible
join :: (Monad f, Monad g) => f (g (f (g a))) -> f (g a)
