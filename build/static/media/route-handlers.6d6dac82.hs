-- Route Handlers ----------------------------------------------------
routeAction1 :: ActionT ()
routeAction1 = do
  request <- lift ask
  lift . lift $ modify (const $ textResponse request "Hello from Route 1")

routeAction2 :: ActionT ()
routeAction2 = throwError "Error in Route 2"

notFound :: Application
notFound request = "Hello from the DEFAULT route"

textResponse :: String -> String -> String
textResponse req msg = unwords ["Request:", req, "\nResponse:", msg]
----------------------------------------------------------------------
