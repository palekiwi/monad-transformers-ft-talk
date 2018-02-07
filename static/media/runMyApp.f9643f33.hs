-- Running the App ---------------------------------------------------
runMyApp :: (Request -> Response) -> AppState -> Request -> Response
runMyApp defHandler appState =
  foldl (flip ($)) defHandler (routes appState)

userInputLoop :: AppState -> IO ()
userInputLoop appState = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp notFound appState request
    putStrLn response
    userInputLoop appState
----------------------------------------------------------------------
