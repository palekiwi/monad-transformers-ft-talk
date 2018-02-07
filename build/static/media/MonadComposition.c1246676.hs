(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)

func1, func2, func3 :: String -> Maybe String
func1 >=> func2 >=> func3

doState :: State Int Int
doState = put 1 >> modify (+1) >> get
