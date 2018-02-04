type Request = String
type Response = String
type Application = Request -> Response

type Route = Application -> Application

newtype AppState = AppState { routes :: [Route] }
type AppStateT = State AppState
