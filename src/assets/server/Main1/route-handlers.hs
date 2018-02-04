constructResponse :: String -> String -> String
constructResponse req msg = unwords ["Request:", req, "\nResponse:", msg]

routeHandler1 :: Request -> Response
routeHandler1 request = constructResponse request "Hello from Route 1"

routeHandler2 :: Request -> Response
routeHandler2 request = constructResponse request "Hello from Route 2"

defaultRoute :: Request -> Response
defaultRoute request = constructResponse request "Hello from the DEFAULT route"
