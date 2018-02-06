-- Route Handlers ----------------------------------------------------
routeAction1 :: Request -> Response
routeAction1 request = return $ textResponse request "Hello from Route 1"

routeAction2 :: Request -> Response
routeAction2 request = Nothing

defaultRoute :: Request -> Response
defaultRoute request = return $ textResponse request "Hello from the DEFAULT route"

textResponse :: String -> String -> String
textResponse req msg = unwords ["Request:", req, "\nResponse:", msg]
----------------------------------------------------------------------
