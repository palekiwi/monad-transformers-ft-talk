newtype Writer w a =
  Writer { runWriter :: (a, w) }
