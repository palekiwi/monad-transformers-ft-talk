join :: (Monad m) => m (m a) -> m a

join :: (Monad f, Monad g) => f (g (f (g a))) -> f (g a)

join' :: (Monad f) => f (Maybe (f (Maybe a))) -> f (Maybe a)
join' f m = case m of
             Nothing -> return Nothing
             Just fma -> do
               ma <- fma
               case ma of
                 Nothing -> return Nothing
                 Just a -> return $ Just a
