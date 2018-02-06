userInputLoop :: AppState -> IO ()
userInputLoop app_state = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp defaultRoute app_state request
    putStrLn response
    userInputLoop app_state
