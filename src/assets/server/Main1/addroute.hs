addRoute :: String -> (Request -> Response) -> AppStateT ()
addRoute pat mf = modify $ \s -> addRoute' (route pat mf) s

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: String -> (Request -> Response)
      -> ((Request -> Response) -> (Request -> Response))
route pat routehandler mw1 request =
  let tryNext = mw1 request in
  if pat == request
  then
    routehandler request
  else
    tryNext
