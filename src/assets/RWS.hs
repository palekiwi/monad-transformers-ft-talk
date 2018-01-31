newtype RWS r w s a =
  RWS { runRWS :: (r -> s -> (a, s, w)) }
