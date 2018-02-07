-- Route Handlers ----------------------------------------------------
routeAction1 :: Request -> Response
routeAction1 request = textResponse request "Hello from Route 1"

routeAction2 :: Request -> Response
routeAction2 request = textResponse request "Hello from Route 2"

notFound :: Request -> Response
notFound request = textResponse request "Hello from the DEFAULT route"

textResponse :: String -> String -> String
textResponse req msg = unwords ["Request:", req, "\nResponse:", msg]
----------------------------------------------------------------------
