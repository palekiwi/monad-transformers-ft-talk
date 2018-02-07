-- Adding Routes -----------------------------------------------------
addRoute :: String -> (Request -> Response) -> AppStateT ()
addRoute pat action = modify $ \s -> addRoute' (route pat action) s

addRoute' :: Middleware -> AppState -> AppState
addRoute' m s@AppState {routes = ms} = s {routes = m:ms}

route :: String -> (Request -> Response) -> Middleware
route pat action nextApp req =
  let tryNext = nextApp req in
  if pat == req
  then
    action req
  else
    tryNext
----------------------------------------------------------------------
