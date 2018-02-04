route :: String -> (Request -> Response)
      -> ((Request -> Response) -> (Request -> Response))
route pat routehandler mw1 request =
  let tryNext = mw1 request in
  if pat == request
  then
    routehandler request
  else
    tryNext

runMyApp :: (Request -> Response) -> AppState -> Request -> Response
runMyApp def app_state =
  foldl (flip ($)) def (routes app_state)
