main :: IO ()
main = do
  maybeUserName <- readUserName
  case maybeUserName of
    Nothing -> print "Invalid user name!"
    Just uName -> do
      maybeEmail <- readEmail
      case maybeEmail of
        Nothing -> print "Invalid email!"
        Just email -> do
          maybePassword <- readPassword
          case maybePassword of
            Nothing -> print "Invalid password"
            Just password -> login uName email password

readUserName :: IO (Maybe String)
readUserName = do
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail :: IO (Maybe String)

readPassword :: IO (Maybe String)

login :: String -> String -> String -> IO ()
