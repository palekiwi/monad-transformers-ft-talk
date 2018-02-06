-- Route Handlers -------------------------------
routeAction1 :: Request -> ActionT
routeAction1 request = return $
  textResponse request "Hello from Route 1"

routeAction2 :: Request -> ActionT
routeAction2 request = throwError "Error in Route 2"

notFound :: Request -> Response
notFound request = textResponse request "Hello from the DEFAULT route"

textResponse :: String -> String -> String
textResponse req msg = unwords ["Request:", req, "\nResponse:", msg]
-------------------------------------------------
