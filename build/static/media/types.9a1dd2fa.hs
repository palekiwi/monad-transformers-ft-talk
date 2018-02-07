type Request = String
type Response = String

type Application = Request -> Response
type Middleware = Application -> Application

type ActionT a = ExceptT ActionError (ReaderT Request (State Response)) a
type ActionError = String

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState
