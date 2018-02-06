type Request = String
type Response = Maybe String

type Application = Request -> Response
type Middleware = Application -> Application

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState
