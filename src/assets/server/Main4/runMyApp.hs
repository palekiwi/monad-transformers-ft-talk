-- Running the App ---------------------------------------------------
runMyApp :: Application -> AppState -> Application
runMyApp defHandler appState =
  foldl (flip ($)) defHandler (routes appState)

userInputLoop :: AppState -> IO ()
userInputLoop appState = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp notFound appState request
    putStrLn response
    putStrLn "---"
    userInputLoop appState
----------------------------------------------------------------------
