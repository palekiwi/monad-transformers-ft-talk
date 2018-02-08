type Request = String
type Response = String

type Application = Request -> Response
type Middleware = Application -> Application

type ActionT = ExceptT ActionError (Reader Request) Response
type ActionError = String

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState
