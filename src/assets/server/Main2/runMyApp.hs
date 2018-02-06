-- Running the App ---------------------------------------------------
runMyApp :: (Request -> Response) -> AppState -> Request -> Response
runMyApp defHandler appState =
  foldl (flip ($)) defHandler (routes appState)

userInputLoop :: AppState -> IO ()
userInputLoop appState = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp defaultRoute appState request
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    userInputLoop appState
----------------------------------------------------------------------
