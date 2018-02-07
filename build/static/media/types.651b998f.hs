type Request = String
type Response = String

type Application = Request -> Response
type Middleware = Application -> Application

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState
