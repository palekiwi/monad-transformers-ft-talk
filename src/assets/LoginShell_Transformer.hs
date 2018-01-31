main :: IO ()
main = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserName
    email <- readEmail
    pass <- readPassword
    return (usr, email, pass)
  case maybeCreds of
    Nothing -> print "Login denied!"
    Just (u, e, p) -> login u e p

readUserName :: MaybeT IO String
readUserName = MaybeT $ do
  str <- getLIne
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail :: MaybeT IO String

readPassword :: MaybeT IO String
